{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Invitation (routeInvitations) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.HVect                   (HVect, ListContains)
import qualified Database.Persist             as P
import           Formatting                   ((%), int, sformat)
import           Network.HTTP.Types.Status    (created201)
import           Web.Spock

import qualified Crypto
import qualified Mail
import           Model.CoreTypes              (ApiAction, Api, Email, apiCfg)
import qualified Model.SqlTypes               as SqlT
import qualified Model.JsonTypes.Invitation   as JsonInvitation
import qualified Model.JsonTypes.InvitationIn as JsonInvitationIn
import           Util                         ( errorJson, JsonError(..)
                                              , eitherJsonBody, emptyResponse)
import           Query.Util                   ( runSQL, integerKey
                                              , trySqlGet, trySqlSelectFirst
                                              , trySqlSelectFirst')
import           Query.Invitation
import           Web.Auth                     (authHook)

routeInvitations :: Api (HVect xs)
routeInvitations =
  prehook authHook $ do
    get "invitations" getInvitationsAction
    delete ("invitations" <//> var) $ \invitationId ->
      trySqlGet invitationId >> deleteInvitationAction invitationId
    patch ("invitations" <//> var) $ \invitationId ->
      trySqlSelectFirst SqlT.InvitationId invitationId
      >>= resendInvitationAction
    post "invitations" (eitherJsonBody >>= postInvitationAction)

getInvitationsAction :: ListContains n Email xs => ApiAction (HVect xs) a
getInvitationsAction = do
  allInvitations <- runSQL $ getInvitations
  json $ map JsonInvitation.jsonInvitation allInvitations

deleteInvitationAction :: ListContains n Email xs
                       => SqlT.InvitationId -> ApiAction (HVect xs) a
deleteInvitationAction invitationId = do
  runSQL $ P.delete invitationId
  emptyResponse

resendInvitationAction :: ListContains n Email xs
                       => P.Entity SqlT.Invitation -> ApiAction (HVect xs) a
resendInvitationAction i@(P.Entity _id invitation) = do
  cfg <- apiCfg <$> getState
  liftIO $ Mail.sendBuiltMail cfg email $ Mail.buildInvitationMail invitationCode
  json $ JsonInvitation.jsonInvitation i
    where
  email = SqlT.invitationEmail invitation
  invitationCode = SqlT.invitationCode invitation

postInvitationAction :: ListContains n Email xs
                     => JsonInvitationIn.Invitation -> ApiAction (HVect xs) a
postInvitationAction invitation = do
  invitationCode <- liftIO Crypto.invitationCode
  let email = JsonInvitationIn.email invitation
  maybeInvitationId <- runSQL $ P.insertUnique
    SqlT.Invitation { SqlT.invitationEmail = email
                    , SqlT.invitationCode  = invitationCode
                    }
  case maybeInvitationId of
    Nothing ->
      errorJson InvitationEmailExists
    Just invitationId -> do
      newInvitation <- trySqlSelectFirst' SqlT.InvitationId invitationId
      cfg <- apiCfg <$> getState
      liftIO $ Mail.sendBuiltMail cfg email (Mail.buildInvitationMail invitationCode)
      setStatus created201
      let location = sformat ("/invitation/" % int) (integerKey invitationId :: Integer)
      setHeader "Location" location
      json . JsonInvitation.jsonInvitation $ newInvitation
