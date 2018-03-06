{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation where

import           Control.Monad.IO.Class       (liftIO)
import           Crypto.Random                (getRandomBytes)
import qualified Database.Persist             as P
import           Formatting                   ((%), int, sformat)
import           Network.HTTP.Types.Status    (created201)
import           Web.Spock
import           Model.CoreTypes              (ApiAction, Api)
import qualified Model.SqlTypes               as SqlT
import qualified Model.JsonTypes.Invitation   as JsonInvitation
import qualified Model.JsonTypes.InvitationIn as JsonInvitationIn
import           Util                         (errorJson, runSQL)
import qualified Util

-- TODO restrict all endpoints to logged in users
routeInvitations :: Api ctx
routeInvitations = do
  get "invitations" getInvitationsAction
  delete ("invitations" <//> var) $ \invitationId ->
    Util.trySqlGet invitationId >> deleteInvitationAction invitationId
  patch ("invitations" <//> var) $ \invitationId ->
    Util.trySqlSelectFirst SqlT.InvitationId invitationId >>= resendInvitation
  post "invitations" (Util.eitherJsonBody >>= postInvitationAction)

getInvitationsAction :: ApiAction ctx a
getInvitationsAction = do
  allInvitations <- runSQL $ P.selectList [] [P.Asc SqlT.InvitationId]
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: SqlT.InvitationId -> ApiAction ctx a
deleteInvitationAction invitationId = do
  runSQL $ P.delete invitationId
  Util.emptyResponse

resendInvitation :: P.Entity SqlT.Invitation -> ApiAction ctx a
resendInvitation =
  -- TODO resend invitation mail
  json . JsonInvitation.jsonInvitation

postInvitationAction :: JsonInvitationIn.Invitation -> ApiAction ctx a
postInvitationAction invitation = do
  invitationCode <- Util.makeHex <$> liftIO (getRandomBytes 10)
  maybeInvitationId <- runSQL $ P.insertUnique
    SqlT.Invitation { SqlT.invitationEmail = JsonInvitationIn.email invitation
                    , SqlT.invitationCode  = invitationCode
                    }
  case maybeInvitationId of
    Nothing ->
      errorJson Util.InvitationEmailExists
    Just invitationId -> do
      newInvitation <- Util.trySqlSelectFirst' SqlT.InvitationId invitationId
      -- TODO send invitation email if smtp config is set
      setStatus created201
      let location = sformat ("/invitation/" % int) (Util.integerKey invitationId :: Integer)
      setHeader "Location" location
      json . JsonInvitation.jsonInvitation $ newInvitation
