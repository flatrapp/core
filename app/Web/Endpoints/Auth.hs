{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Auth where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                       hiding (json)
import           Data.HVect                       hiding (pack)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Time.Clock.POSIX            (POSIXTime, getPOSIXTime,
                                                   posixSecondsToUTCTime)
import qualified Database.Persist                 as P
import           Database.Persist.Sql             hiding (delete, get)
import           Database.Persist.Sqlite          (SqlBackend)
import           Model.CoreTypes
import           Model.JsonTypes.LoginCredentials
import           Network.HTTP.Types.Status
import           System.Random
import qualified Util
import qualified Web.JWT                          as JWT
import           Web.Spock

routeAuth = do
  post "auth" $ do
    maybeLogin <- jsonBody :: ApiAction ctx (Maybe LoginCredentials)
    case maybeLogin of
      Nothing -> do
        setStatus badRequest400
        Util.errorJson Util.InvalidRequest
      Just loginCredentials -> do
        currentTime <- liftIO getPOSIXTime
        let validFor = tokenTimeout + tokenGracePeriod
        let validUntil = validFor + currentTime
        gen <- liftIO $ getStdGen
        let tokenId =  Util.randomText 64 gen
        let key = JWT.secret jwtSecret
        let cs = JWT.def { JWT.exp = JWT.numericDate validUntil
                         , JWT.jti = JWT.stringOrURI tokenId
                         , JWT.sub = JWT.stringOrURI $ email loginCredentials
                         }
        maybeUser<- Util.runSQL $ P.selectFirst [UserEmail ==. email loginCredentials] []
        -- TODO check password and throw invalid credentials instead of
        -- this user does not exist
        case maybeUser of
          Nothing -> do
            setStatus forbidden403
            Util.errorJson Util.UserPasswordWrong
          Just (Entity userId _) -> do
            newId <- Util.runSQL $ insert $ Token userId tokenId $ posixSecondsToUTCTime validUntil
            json $ object [ "token"    .= JWT.encodeSigned JWT.HS256 key cs
                          , "tokenId"  .= tokenId
                          , "validFor" .= validFor
                          ]

tokenTimeout :: Data.Time.Clock.POSIX.POSIXTime
tokenTimeout = 60 * 60

tokenGracePeriod :: Data.Time.Clock.POSIX.POSIXTime
tokenGracePeriod = 60

jwtSecret :: Text
jwtSecret = "secret"
