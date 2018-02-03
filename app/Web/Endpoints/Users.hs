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

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                    as T
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

returnUserById userId = do
  maybeUser <- runSQL $ P.selectFirst [SqlT.UserId ==. userId] []
  fromMaybe
    (do setStatus notFound404
        errorJson Util.UserNotFound)
    (json . JsonUser.jsonUser <$> maybeUser)

routeUsers = do
  get "users" $ do
    allUsers <- runSQL $ selectList [] [Asc SqlT.UserId]
    json $ map JsonUser.jsonUser allUsers
  get ("users" <//> var) $ \userId ->
    returnUserById userId
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
        case JsonRegistration.code registration of
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
                newId <- registerUser registration gen (SqlT.invitationEmail theInvitation) True
                returnUserById newId
          Nothing -> do
            gen <- liftIO getStdGen
            case JsonRegistration.email registration of
              Just email ->
                if email `elem` [T.pack "ds@test.com"] then do  -- TODO via config
                  setStatus created201
                  newId <- registerUser registration gen email True
                  returnUserById newId
                else do
                  maybeInvitation <- runSQL $ P.selectFirst [SqlT.InvitationEmail ==. email] [] 
                  case maybeInvitation of
                    Just (Entity _invitationId _invitation) -> do
                      -- TODO send verification Email if smtp config set
                      -- $frontedUrl/#signup?code=$code&serverUrl=$serverUrl
                      newId <- registerUser registration gen email False
                      returnUserById newId
                    Nothing -> do
                      -- User provided only email but is not invited
                      setStatus unauthorized401
                      Util.errorJson Util.NotInvited
              Nothing -> do
                -- User should provide at least code or email
                setStatus badRequest400
                Util.errorJson Util.BadRequest  -- TODO check what it returns

-- TODO check that user is not there
registerUser registration gen mail verified = runSQL $ insert user
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
