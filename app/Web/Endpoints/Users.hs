{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Endpoints.Users where

import qualified Config                       as Cfg
import           Control.Monad.IO.Class
import           Data.Maybe
import           Database.Persist             hiding (delete, get)
import qualified Database.Persist             as P
import qualified Model.CoreTypes              as SqlT
import qualified Model.JsonTypes.Registration as JsonRegistration
import qualified Model.JsonTypes.User         as JsonUser
import           Network.HTTP.Types.Status
import           System.Random
import           Util                         (errorJson, runSQL)
import qualified Util
import           Web.Spock

returnUserById Nothing = do
  -- TODO combine with registration... because this function is also called once when there is no registration happening
  setStatus conflict409
  Util.errorJson Util.UserEmailExists
returnUserById (Just userId ) = do
  maybeUser <- runSQL $ P.selectFirst [SqlT.UserId ==. userId] []
  fromMaybe
    (do setStatus notFound404
        errorJson Util.UserNotFound)
    (json . JsonUser.jsonUser <$> maybeUser)

routeUsers cfg = do
  get "users" $
    json =<< (map JsonUser.jsonUser <$> runSQL (selectList [] [Asc SqlT.UserId]))
  get ("users" <//> var) $ \userId ->
    returnUserById $ Just userId
  delete ("user" <//> var) $ \(userId :: SqlT.UserId) -> do
    maybeUser <- runSQL $ P.get userId :: SqlT.ApiAction ctx (Maybe SqlT.User)
    case maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson Util.UserNotFound
      Just _theUser -> do
        runSQL $ P.delete userId
        setStatus noContent204
        text ""  -- TODO check if empty body is possible
  -- TOOD implement put "users" $ do
  post "users" $ do
    maybeRegistration <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonRegistration.Registration)
    case maybeRegistration of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest  -- malformed json
      Just registration ->
        case JsonRegistration.invitationCode registration of
          Just code -> do
            maybeInvitation <- runSQL $ P.selectFirst
              [SqlT.InvitationCode ==. Just code] []
            case maybeInvitation of
              Nothing -> do
                setStatus notFound404
                errorJson Util.InvitationCodeInvalid  -- code not in DB
              Just (Entity invitationId theInvitation) -> do
                runSQL $ P.updateWhere [SqlT.InvitationId ==. invitationId] []
                setStatus created201
                gen <- liftIO getStdGen
                let email = SqlT.invitationEmail theInvitation
                -- check if user exists
                maybeUser <- runSQL $ P.selectFirst [SqlT.UserEmail ==. email] []
                case maybeUser of
                  Just _user -> do
                    setStatus conflict409
                    Util.errorJson Util.UserEmailExists
                  Nothing -> do
                    newId <- registerUser registration gen email True
                    returnUserById newId
          Nothing -> do
            gen <- liftIO getStdGen
            case JsonRegistration.email registration of
              Just email ->
                if email `elem` Cfg.whitelistedMails cfg then do
                  setStatus created201
                  newId <- registerUser registration gen email True
                  returnUserById newId
                else do
                  maybeInvitation <- runSQL $ P.selectFirst [SqlT.InvitationEmail ==. email] []
                  case maybeInvitation of
                    Just (Entity _invitationId _invitation) -> do
                      -- check if user exists
                      maybeUser <- runSQL $ P.selectFirst [SqlT.UserEmail ==. email] []
                      case maybeUser of
                        Just _user -> do
                          setStatus conflict409
                          Util.errorJson Util.UserEmailExists
                        Nothing ->
                          -- TODO send verification Email if smtp config set
                          -- $frontedUrl/#signup?code=$code&serverUrl=$serverUrl
                          registerUser registration gen email False
                            >>= returnUserById
                    Nothing -> do
                      -- User provided only email but is not invited
                      setStatus unauthorized401
                      Util.errorJson Util.NotInvited
              Nothing -> do
                -- User should provide at least code or email
                setStatus badRequest400
                Util.errorJson Util.BadRequest  -- TODO check what it returns

-- TODO check that user is not there
registerUser registration gen mail verified = runSQL $ insertUnique user
    where user = SqlT.User
                    { SqlT.userEmail     = mail
                    , SqlT.userPassword  = hashedSaltedPassword
                    , SqlT.userSalt      = Util.makeHex salt'
                    , SqlT.userFirstName = JsonRegistration.firstName registration
                    , SqlT.userLastName  = JsonRegistration.lastName registration
                    , SqlT.userVerified  = verified
                    }
          pw = JsonRegistration.password registration
          salt' = Util.randomBS 512 gen
          hashedSaltedPassword = Util.hashPassword pw salt'
