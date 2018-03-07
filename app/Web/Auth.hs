{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Auth (authHook, getCurrentUser) where

import           Control.Monad.IO.Class (liftIO)
import           Data.HVect             (HVect(..), ListContains, findFirst)
import qualified Database.Persist       as P
import           Database.Persist.Sql   (Entity(..), SqlBackend)
import           Data.Text              (Text, stripPrefix, pack)
import           Data.Time.Clock        (getCurrentTime, UTCTime)
import qualified Web.JWT                as JWT
import           Web.Spock

import qualified Model.CoreTypes        as CoreT
import           Model.SqlTypes         ( User
                                        , Token
                                        , EntityField(..)
                                        , tokenValidUntil
                                        )
import           Web.Endpoints.Auth     (jwtSecret)
import qualified Util

getCurrentUser :: ListContains n CoreT.Email xs
               => CoreT.ApiAction (HVect xs) (P.Entity User)
getCurrentUser = do
  email <- fmap findFirst getContext
  Util.trySqlSelectFirst' UserEmail email

authHook :: CoreT.ApiAction (HVect xs) (HVect (CoreT.Email ': xs))
authHook = do
  oldCtx <- getContext
  maybeAuthHeader <- header "Authorization"
  currentTime <- liftIO getCurrentTime
  let maybeClaims = maybeAuthHeader >>= tokenFromHeader >>= extractToken
  let maybeTokenId = JWT.jti =<< maybeClaims
  let maybeSubject = JWT.sub =<< maybeClaims

  (maybeT::Maybe Token) <- conditionalTokenEntityUnpack (validateToken currentTime) <$> retrieveServerToken maybeTokenId

  case maybeT >> maybeSubject of
    Nothing ->
      Util.errorJson Util.Unauthorized
    Just subject ->
      return $ (pack . show $ subject) :&: oldCtx

tokenFromHeader :: Text -> Maybe Text
tokenFromHeader = stripPrefix "Bearer "

extractToken :: JWT.JSON -> Maybe JWT.JWTClaimsSet
extractToken bearerToken = -- TODO use secret specified in config
  JWT.claims <$> JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken

retrieveServerToken :: (SpockConn m ~ SqlBackend, Monad m, HasSpock m) =>
                       Maybe JWT.StringOrURI -> m (Maybe (Entity Token))
retrieveServerToken (Just tokenId) =
  Util.runSQL $ P.selectFirst [TokenTokenId P.==. Util.showText tokenId] []
retrieveServerToken Nothing = return Nothing

validateToken :: UTCTime -> Token -> Maybe Token
validateToken currentTime token
  | tokenValidUntil token > currentTime = Just token
  | otherwise = Nothing

conditionalTokenEntityUnpack :: (Token -> Maybe Token) -> Maybe (Entity Token) -> Maybe Token
conditionalTokenEntityUnpack = flip ((>>=) . fmap entityVal)
