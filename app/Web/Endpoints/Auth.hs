{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Auth where

import           Control.Monad.IO.Class
import           Crypto.Random
import           Data.Aeson                       hiding (json)
import           Data.Text                        (Text)
import           Data.Time.Clock.POSIX            (POSIXTime, getPOSIXTime,
                                                   posixSecondsToUTCTime)
import qualified Database.Persist                 as P
import           Database.Persist.Sql             hiding (delete, get)
import qualified Model.CoreTypes                  as SqlT
import           Model.JsonTypes.LoginCredentials
import           Network.HTTP.Types.Status
import qualified Util
import qualified Web.JWT                          as JWT
import           Web.Spock

routeAuth :: SqlT.Api ctx
routeAuth =
  post "auth" $ jsonBody >>= postAuthAction

postAuthAction :: Maybe LoginCredentials -> SqlT.ApiAction ctx a
postAuthAction Nothing = do
  setStatus badRequest400
  Util.errorJson Util.InvalidRequest
postAuthAction (Just loginCredentials) = do
  maybeUser <- Util.runSQL $ P.selectFirst [SqlT.UserEmail ==. email loginCredentials] []  -- TODO chain this somehow with the Maybe LoginCredentials
  case maybeUser of
    Nothing -> do
      setStatus forbidden403
      Util.errorJson Util.CredentialsWrong
    Just (Entity userId user) -> do
      let hashedPw = Util.hashPassword (password loginCredentials) (Util.decodeHex . SqlT.userSalt $ user)
      if hashedPw /= SqlT.userPassword user then do
        setStatus forbidden403
        Util.errorJson Util.CredentialsWrong
      else if not $ SqlT.userVerified user then do
        setStatus forbidden403
        Util.errorJson Util.EmailNotVerified
      else do
        currentTime <- liftIO getPOSIXTime
        let validUntil = currentTime + tokenTimeout + tokenGracePeriod
        tokenId <- Util.makeHex <$> liftIO (getRandomBytes 10)
        let key = JWT.secret jwtSecret
        let cs = JWT.def { JWT.exp = JWT.numericDate validUntil
                         , JWT.jti = JWT.stringOrURI tokenId
                         , JWT.sub = JWT.stringOrURI $ email loginCredentials
                         }
        _newId <- Util.runSQL . insert $ SqlT.Token userId tokenId $ posixSecondsToUTCTime validUntil
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
