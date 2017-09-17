{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Users where

import           Network.HTTP.Types.Status
import           Web.Spock

import           Control.Monad.IO.Class
import           Data.Aeson                   hiding (json)
import           Data.Text                    (Text)
import           Database.Persist             hiding (delete, get)
import qualified Database.Persist             as P
import           System.Random

import qualified Model.CoreTypes              as SqlT
import qualified Model.JsonTypes.Registration as JsonRegistration
import qualified Model.JsonTypes.User         as JsonUser
import           Util                         (errorJson, runSQL)
import qualified Util

routeUsers = do
  get "users" $ do
    allUsers <- runSQL $ selectList [] [Asc SqlT.UserId]
    json $ map JsonUser.jsonUser allUsers
  get ("users" <//> var) $ \userId -> do
    maybeUser <- runSQL $ P.selectFirst [SqlT.UserId ==. userId] []
    case JsonUser.jsonUser <$> maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching id"
      Just theUser -> json theUser
  get ("users" <//> var) $ \(email :: Text) -> do
    maybeUser <- runSQL $ P.selectFirst [SqlT.UserEmail ==. email] []
    case JsonUser.jsonUser <$> maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching email"
      Just theUser -> json theUser
  delete ("user" <//> var) $ \(userId :: SqlT.UserId) -> do
    maybeUser <- runSQL $ P.get userId :: SqlT.ApiAction ctx (Maybe SqlT.User)
    case maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching id"
      Just _theUser -> do
        runSQL $ P.delete userId
        text "Thanks for deleting the user"
  post "users" $ do
    maybeRegistration <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonRegistration.Registration)
    case maybeRegistration of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as User"
      Just registration -> do
        setStatus created201
        gen <- liftIO getStdGen
        newId <- registerUser registration gen
        json $ object ["result" .= String "success", "id" .= newId]

registerUser registration gen = runSQL $ insert user
    where user = SqlT.User
                    { SqlT.userEmail     = JsonRegistration.email registration
                    , SqlT.userPassword  = hashedSaltedPassword
                    , SqlT.userSalt      = Util.makeHex salt'
                    , SqlT.userFirstName = JsonRegistration.firstName registration
                    , SqlT.userLastName  = JsonRegistration.lastName registration
                    }
          pw = JsonRegistration.password registration
          salt' = Util.randomBS 512 gen
          hashedSaltedPassword = Util.hashPassword pw salt'
