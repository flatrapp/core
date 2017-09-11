{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Users where

import           Network.HTTP.Types.Status
import           Web.Spock

import           Data.Aeson                hiding (json)
import           Data.Text                 (Text)
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import           Database.Persist.Sql      hiding (delete, get)

import           Model.CoreTypes
import qualified Model.JsonTypes.User      as JsonTypes
import           Util                      (errorJson, runSQL)

integerKey :: (Num n, ToBackendKey SqlBackend record) => Key record -> n
integerKey = fromIntegral . fromSqlKey

jsonUser :: Entity User -> JsonTypes.User
jsonUser (Entity userId user) = JsonTypes.User
    { JsonTypes.id        = integerKey userId
    , JsonTypes.email     = userEmail user
    , JsonTypes.firstName = userFirstName user
    , JsonTypes.lastName  = userLastName user
    }

routeUsers = do
  get "users" $ do
    allUsers <- runSQL $ selectList [] [Asc UserId]
    json allUsers
  get ("users" <//> var) $ \userId -> do
    maybeUser <- runSQL $ P.selectFirst [UserId ==. userId] []
    case jsonUser <$> maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching id"
      Just theUser -> json theUser
  get ("users" <//> var) $ \(email :: Text) -> do
    maybeUser <- runSQL $ P.selectFirst [UserEmail ==. email] []
    case jsonUser <$> maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching email"
      Just theUser -> json theUser
  delete ("user" <//> var) $ \(userId :: UserId) -> do
    maybeUser <- runSQL $ P.get userId :: ApiAction ctx (Maybe User)
    case maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a user with matching id"
      Just _theUser -> do
        runSQL $ P.delete userId
        text "Thanks for deleting the user"
  post "users" $ do
    maybeRegistration <- jsonBody :: ApiAction ctx (Maybe User)
    case maybeRegistration of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as User"
      Just user -> do
        setStatus created201
        newId <- runSQL $ insert user
        json $ object ["result" .= String "success", "id" .= newId]
