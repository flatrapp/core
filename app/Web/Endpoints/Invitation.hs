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
import qualified Model.CoreTypes            as SqlT
import qualified Model.JsonTypes.Invitation as JsonInvitation
import           Util                       (errorJson, runSQL)
import qualified Util

routeInvitations :: SqlT.Api ctx
routeInvitations = do
  get "invitations" getInvitationAction
  delete ("invitations" <//> var) $ \invitationId ->
    runSQL (P.get invitationId) >>= deleteInvitationAction invitationId
  patch ("invitations" <//> var) resendInvitation
  post "invitations" (jsonBody >>= postInvitationAction)

getInvitationAction :: SqlT.ApiAction ctx a
getInvitationAction = do
  allInvitations <- runSQL $ selectList [] [Asc SqlT.InvitationId]
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: SqlT.InvitationId -> Maybe SqlT.Invitation -> SqlT.ApiAction ctx a
deleteInvitationAction _ Nothing =
  errorJson Util.NotFound
deleteInvitationAction invitationId (Just _theInvitation) = do
  runSQL $ P.delete invitationId
  Util.emptyResponse

resendInvitation :: SqlT.InvitationId -> SqlT.ApiAction ctx a
resendInvitation invitationId  = do
  mInvitation <- runSQL $ P.selectFirst [SqlT.InvitationId ==. invitationId] []
  case mInvitation of
    Nothing -> errorJson Util.NotFound
    Just invitation -> do
      -- TODO resend invitation mail
     json . JsonInvitation.jsonInvitation $ invitation

postInvitationAction :: Maybe JsonInvitation.Invitation -> SqlT.ApiAction ctx a
postInvitationAction Nothing =
  errorJson Util.BadRequest
postInvitationAction (Just invitation) = do
  invitationCode <- Util.makeHex <$> liftIO (getRandomBytes 10)
  maybeInvitationId <- runSQL $ insertUnique
    SqlT.Invitation { SqlT.invitationEmail = JsonInvitation.email invitation
                    , SqlT.invitationCode  = Just invitationCode
                    }
  case maybeInvitationId of
    Nothing ->
      errorJson Util.InvitationEmailExists
    Just invitationId -> do
      maybeInvitation' <- runSQL $ selectFirst [SqlT.InvitationId ==. invitationId] []
      case JsonInvitation.jsonInvitation <$> maybeInvitation' of
        Nothing -> error "I fucked up #2"
        Just theInvitation -> do
          -- TODO send invitation email if smtp config is set
          setStatus created201
          let location :: T.Text = T.pack $ printf "/invitation/%d" (Util.integerKey invitationId :: Integer)
          setHeader "Location" location
          json theInvitation
