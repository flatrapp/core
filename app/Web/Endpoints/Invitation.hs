{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation where

import           Control.Monad.IO.Class       (liftIO)
import           Crypto.Random                (getRandomBytes)
import           Data.HVect                   (HVect, ListContains)
import qualified Database.Persist             as P
import           Formatting                   ((%), int, sformat)
import           Network.HTTP.Types.Status    (created201)
import           Web.Spock

import           Model.CoreTypes              (ApiAction, Api, Email)
import qualified Model.SqlTypes               as SqlT
import qualified Model.JsonTypes.Invitation   as JsonInvitation
import qualified Model.JsonTypes.InvitationIn as JsonInvitationIn
import           Util                         (errorJson, runSQL)
import qualified Util
import           Web.Auth                  (authHook)

-- TODO restrict all endpoints to logged in users
routeInvitations :: Api (HVect xs)
routeInvitations =
  prehook authHook $ do
    get "invitations" getInvitationsAction
    delete ("invitations" <//> var) $ \invitationId ->
      Util.trySqlGet invitationId >> deleteInvitationAction invitationId
    patch ("invitations" <//> var) $ \invitationId ->
      Util.trySqlSelectFirst SqlT.InvitationId invitationId
      >>= resendInvitationAction
    post "invitations" (Util.eitherJsonBody >>= postInvitationAction)

getInvitationsAction :: ListContains n Email xs => ApiAction (HVect xs) a
getInvitationsAction = do
  allInvitations <- runSQL $ P.selectList [] [P.Asc SqlT.InvitationId]
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: ListContains n Email xs
                       => SqlT.InvitationId -> ApiAction (HVect xs) a
deleteInvitationAction invitationId = do
  runSQL $ P.delete invitationId
  Util.emptyResponse

resendInvitationAction :: ListContains n Email xs
                       => P.Entity SqlT.Invitation -> ApiAction (HVect xs) a
resendInvitationAction =
  -- TODO resend invitation mail
  json . JsonInvitation.jsonInvitation

postInvitationAction :: ListContains n Email xs
                     => JsonInvitationIn.Invitation -> ApiAction (HVect xs) a
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
