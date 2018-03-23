{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Auth
    ( routeAuth
    , jwtSecret -- TODO get directly from config
    , tokenTimeout
    , tokenGracePeriod
    )
where

import           Control.Monad                    (when, unless)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (object, (.=))
import           Data.Text                        (Text)
import           Data.Time.Clock.POSIX            (POSIXTime, getPOSIXTime
                                                  , posixSecondsToUTCTime)
import           Database.Persist.Sql             hiding (delete, get)
import qualified Web.JWT                          as JWT
import           Web.Spock

import qualified Crypto
import           Model.CoreTypes                  (ApiAction, Api)
import qualified Model.SqlTypes                   as SqlT
import           Model.JsonTypes.LoginCredentials
import           Util                             ( errorJson
                                                  , JsonError ( CredentialsWrong
                                                              , EmailNotVerified
                                                              , UserDisabled
                                                              )
                                                  , eitherJsonBody
                                                  )
import           Query.Util                       (trySqlSelectFirstError, runSQL)

routeAuth :: Api ctx
routeAuth = post "auth" $ eitherJsonBody >>= postAuthAction

postAuthAction :: LoginCredentials -> ApiAction ctx a
postAuthAction loginCredentials = do
  Entity userId user <- trySqlSelectFirstError CredentialsWrong
                                               SqlT.UserEmail
                                             $ email loginCredentials
  let hashedPw = Crypto.hashPassword (password loginCredentials)
                                    $ SqlT.userSalt user
  when (hashedPw /= SqlT.userPassword user) $
    errorJson CredentialsWrong
  unless (SqlT.userIsVerified user) $
    errorJson EmailNotVerified
  when (SqlT.userDisabled user) $
    errorJson UserDisabled

  currentTime <- liftIO getPOSIXTime
  tokenId <- liftIO Crypto.tokenId
  let validUntil = currentTime + tokenTimeout + tokenGracePeriod
  let key = JWT.secret jwtSecret
  let cs = JWT.def { JWT.exp = JWT.numericDate validUntil
                    , JWT.jti = JWT.stringOrURI tokenId
                    , JWT.sub = JWT.stringOrURI $ email loginCredentials
                    }
  _newId <- runSQL . insert . SqlT.Token
                userId tokenId $ posixSecondsToUTCTime validUntil
  json $ object [ "token"    .= JWT.encodeSigned JWT.HS256 key cs
                , "tokenId"  .= tokenId
                , "validFor" .= tokenTimeout
                ]

tokenTimeout :: Data.Time.Clock.POSIX.POSIXTime
tokenTimeout = 60 * 60

tokenGracePeriod :: Data.Time.Clock.POSIX.POSIXTime
tokenGracePeriod = 60

jwtSecret :: Text
jwtSecret = "6QQf4YsgAmyJzZFipkC5sMIXMI4hccbqdF8rmlcN"
