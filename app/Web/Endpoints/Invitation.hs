{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation where

import           Control.Monad.IO.Class
import           Crypto.Random
import           Data.Maybe                 (fromMaybe)
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
  patch ("invitations" <//> var) $ \invitationId ->
    jsonBody >>= patchInvitationAction invitationId
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

patchInvitationAction :: SqlT.InvitationId -> Maybe JsonInvitation.Invitation -> SqlT.ApiAction ctx a
patchInvitationAction _ Nothing =
  errorJson Util.BadRequest
patchInvitationAction invitationId (Just invitation) = do
    maybeInvitation <- runSQL $ P.get invitationId
    case maybeInvitation of
      Nothing -> errorJson Util.NotFound
      (Just _invitation) -> do
          runSQL $ P.updateWhere
              [SqlT.InvitationId ==. invitationId]
              [SqlT.InvitationEmail =. JsonInvitation.email invitation]
          newI <- runSQL $ P.selectFirst [SqlT.InvitationId ==. invitationId] []
          -- TODO resend invitation mail
          fromMaybe
             (error "I fucked up #3")
             (json . JsonInvitation.jsonInvitation <$> newI)

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
