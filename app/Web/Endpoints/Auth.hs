{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Auth where

import           Control.Monad.IO.Class           (liftIO)
import           Crypto.Random
import           Data.Aeson                       hiding (json)
import           Data.Text                        (Text)
import           Data.Time.Clock.POSIX            (POSIXTime, getPOSIXTime
                                                  , posixSecondsToUTCTime)
import           Database.Persist.Sql             hiding (delete, get)
import           Model.CoreTypes                  (ApiAction, Api)
import qualified Model.SqlTypes                   as SqlT
import           Model.JsonTypes.LoginCredentials
import           Util                             ( errorJson
                                                  , JsonError ( CredentialsWrong
                                                              , EmailNotVerified
                                                              , UserDisabled
                                                              )
                                                  , eitherJsonBody
                                                  , trySqlSelectFirstError
                                                  )
import qualified Util
import qualified Web.JWT                          as JWT
import           Web.Spock

routeAuth :: Api ctx
routeAuth = post "auth" $ eitherJsonBody >>= postAuthAction

postAuthAction :: LoginCredentials -> ApiAction ctx a
postAuthAction loginCredentials = do
  Entity userId user <- trySqlSelectFirstError CredentialsWrong
                                               SqlT.UserEmail
                                             $ email loginCredentials
  let hashedPw = Util.hashPassword (password loginCredentials)
                                   (Util.decodeHex . SqlT.userSalt $ user)
  if hashedPw /= SqlT.userPassword user then
    errorJson CredentialsWrong
  else if not $ SqlT.userIsVerified user then
    errorJson EmailNotVerified
  else if SqlT.userDisabled user then
    errorJson UserDisabled
  else do
    currentTime <- liftIO getPOSIXTime
    tokenId <- Util.makeHex <$> liftIO (getRandomBytes 10)
    let validUntil = currentTime + tokenTimeout + tokenGracePeriod
    let key = JWT.secret jwtSecret
    let cs = JWT.def { JWT.exp = JWT.numericDate validUntil
                     , JWT.jti = JWT.stringOrURI tokenId
                     , JWT.sub = JWT.stringOrURI $ email loginCredentials
                     }
    _newId <- Util.runSQL . insert . SqlT.Token
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
