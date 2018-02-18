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
  patch ("invitations" <//> var) resendInvitation
  post "invitations" (Util.eitherJsonBody >>= postInvitationAction)

getInvitationsAction :: ApiAction ctx a
getInvitationsAction = do
  allInvitations <- runSQL $ selectList [] [Asc SqlT.InvitationId]
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: SqlT.InvitationId -> ApiAction ctx a
deleteInvitationAction invitationId = do
  runSQL $ P.delete invitationId
  Util.emptyResponse

resendInvitation :: SqlT.InvitationId -> ApiAction ctx a
resendInvitation invitationId  = do
  mInvitation <- runSQL $ P.selectFirst [SqlT.InvitationId ==. invitationId] []
  case mInvitation of
    Nothing -> errorJson Util.NotFound  -- TODO use Util.trySqlGet
    Just invitation ->
      -- TODO resend invitation mail
     json . JsonInvitation.jsonInvitation $ invitation

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
      maybeInvitation' <- runSQL $ selectFirst [SqlT.InvitationId ==. invitationId] []
      case JsonInvitation.jsonInvitation <$> maybeInvitation' of
        Nothing -> error "I fucked up #2"
        Just theInvitation -> do
          -- TODO send invitation email if smtp config is set
          setStatus created201
          let location :: T.Text = T.pack $ printf "/invitation/%d" (Util.integerKey invitationId :: Integer)
          setHeader "Location" location
          json theInvitation
