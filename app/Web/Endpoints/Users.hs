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
    case JsonUser.jsonUser <$> maybeUser of
      Nothing -> do
        setStatus notFound404
        errorJson Util.UserNotFound
      Just theUser -> json theUser

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
        text "Thanks for deleting the user"
  post "users" $ do
    maybeRegistration <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonRegistration.Registration)
    case maybeRegistration of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest
      Just registration ->
        case JsonRegistration.code registration of
          Just code -> do
            maybeInvitation <- runSQL $ P.selectFirst [SqlT.InvitationCode ==. Just code] []
            case maybeInvitation of
              Nothing -> do
                setStatus notFound404
                errorJson Util.InvalidInvitationCode
              Just (Entity _invId theInvitation) -> do
                setStatus created201
                gen <- liftIO getStdGen
                newId <- registerUser registration gen (Just $ SqlT.invitationEmail theInvitation)
                returnUserById newId
          Nothing -> do
            -- TODO send verification Email
            -- $frontedUrl/#signup?code=$code&serverUrl=$serverUrl
            setStatus created201
            gen <- liftIO getStdGen
            newId <- registerUser registration gen Nothing
            returnUserById newId

-- TODO check that user is not there
registerUser registration gen mailOrNot = runSQL $ insert user
    where user = SqlT.User
                    { SqlT.userEmail     = fromMaybe (fromJust $ JsonRegistration.email registration) mailOrNot
                    , SqlT.userPassword  = hashedSaltedPassword
                    , SqlT.userSalt      = Util.makeHex salt'
                    , SqlT.userFirstName = JsonRegistration.firstName registration
                    , SqlT.userLastName  = JsonRegistration.lastName registration
                    , SqlT.userVerified  = isJust mailOrNot
                    }
          pw = JsonRegistration.password registration
          salt' = Util.randomBS 512 gen
          hashedSaltedPassword = Util.hashPassword pw salt'
