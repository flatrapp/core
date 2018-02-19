{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation where

import           Control.Monad.IO.Class
import           Crypto.Random
import qualified Data.Text                  as T
import           Database.Persist           hiding (delete, get)
import qualified Database.Persist           as P
import           Network.HTTP.Types.Status  (created201)
import           Text.Printf
import           Web.Spock
import           Model.CoreTypes            (ApiAction, Api)
import qualified Model.SqlTypes             as SqlT
import qualified Model.JsonTypes.Invitation as JsonInvitation
import           Util                       (errorJson, runSQL)
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
  allInvitations <- runSQL $ selectList [] [Asc SqlT.InvitationId]
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: SqlT.InvitationId -> ApiAction ctx a
deleteInvitationAction invitationId = do
  runSQL $ P.delete invitationId
  Util.emptyResponse

resendInvitation :: Entity SqlT.Invitation -> ApiAction ctx a
resendInvitation =
  -- TODO resend invitation mail
  json . JsonInvitation.jsonInvitation

postInvitationAction :: JsonInvitation.Invitation -> ApiAction ctx a
postInvitationAction invitation = do
  invitationCode <- Util.makeHex <$> liftIO (getRandomBytes 10)
  maybeInvitationId <- runSQL $ insertUnique
    SqlT.Invitation { SqlT.invitationEmail = JsonInvitation.email invitation
                    , SqlT.invitationCode  = Just invitationCode
                    }
  case maybeInvitationId of
    Nothing ->
      errorJson Util.InvitationEmailExists
    Just invitationId -> do
      newInvitation <- Util.trySqlSelectFirst' SqlT.InvitationId invitationId
      -- TODO send invitation email if smtp config is set
      setStatus created201
      let location :: T.Text = T.pack $ printf "/invitation/%d" (Util.integerKey invitationId :: Integer)
      setHeader "Location" location
      json . JsonInvitation.jsonInvitation $ newInvitation
