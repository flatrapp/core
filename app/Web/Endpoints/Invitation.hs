{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation where

import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import           Database.Persist           hiding (delete, get)
import qualified Database.Persist           as P
import           Network.HTTP.Types.Status
import           System.Random
import           Text.Printf
import           Web.Spock

import qualified Model.CoreTypes            as SqlT
import qualified Model.JsonTypes.Invitation as JsonInvitation
import           Util                       (errorJson, runSQL)
import qualified Util

routeInvitations = do
  get "invitations" $ do
    allInvitations <- runSQL $ selectList [] [Asc SqlT.InvitationId]
    json $ map JsonInvitation.jsonInvitation allInvitations
  delete ("invitations" <//> var) $ \(invitationId :: SqlT.InvitationId) -> do
    maybeInvitation' <- runSQL $ P.get invitationId :: SqlT.ApiAction ctx (Maybe SqlT.Invitation)
    case maybeInvitation' of
      Nothing -> do
        setStatus notFound404
        errorJson Util.NotFound
      Just _theInvitation -> do
        setStatus noContent204
        runSQL $ P.delete invitationId  -- TODO check return value
        text ""  -- TODO check if I can send empty response
  post "invitations" $ do
    -- TODO requires authentication only when there already is an invitation
    maybeInvitation <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonInvitation.Invitation)
    case maybeInvitation of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest
      Just invitation -> do
        gen <- liftIO getStdGen
        invitationId <- runSQL $
          insert SqlT.Invitation { SqlT.invitationEmail = JsonInvitation.email invitation
                                 , SqlT.invitationCode  = Just $ Util.randomText 16 gen
                                 }
        maybeInvitation <- runSQL $ selectFirst [SqlT.InvitationId ==. invitationId] []
        case JsonInvitation.jsonInvitation <$> maybeInvitation of
          Nothing -> error "I fucked up #2"
          Just theInvitation -> do
            -- TODO send invitation email if smtp config is set
            setStatus created201
            let location :: T.Text = T.pack $ printf "/invitation/%d" (Util.integerKey invitationId :: Integer)
            setHeader "Location" location
            json theInvitation
