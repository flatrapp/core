{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Web.App where

import           Config                    (FlatrCfg)
import           Control.Monad.IO.Class
import           Data.Aeson                (object, (.=))
import           Data.HVect                hiding (pack)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time.Clock           (getCurrentTime, UTCTime)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Database.Persist          as P
import           Database.Persist.Sql      hiding (delete, get)
import           Database.Persist.Sqlite   (SqlBackend)
import           Model.CoreTypes
import           Network.HTTP.Types.Status (Status, notFound404, statusMessage)
import qualified Util
import           Web.Endpoints.Auth
import           Web.Endpoints.Info
import           Web.Endpoints.Invitation
import           Web.Endpoints.Tasks
import           Web.Endpoints.Users
import qualified Web.JWT                   as JWT
import           Web.Spock

app :: FlatrCfg -> Api ()
app cfg =
  prehook corsHeader $
  prehook initHook $ do
    routeAuth
    routeTasks
    routeInfo
    routeInvitations
    routeUsers cfg
    prehook authHook $ do
      get ("users" <//> "current") currentUserAction  -- TODO move to Endpoints/Users.hs
      get "secret" secretAction
    get "test" testAction
    -- Allow for pre-flight AJAX requests
    hookAny OPTIONS $ \_ -> do
      setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
      setHeader "Access-Control-Allow-Methods" "OPTIONS, GET, POST, PUT, PATCH, DELETE"

testAction :: ApiAction ctx a
testAction = do
  currentTime <- liftIO getPOSIXTime
  text $ pack $ show currentTime <> show (currentTime + tokenTimeout + tokenGracePeriod)

secretAction :: ListContains n Email xs => ApiAction (HVect xs) a
secretAction = do
  (subject :: Text) <- fmap findFirst getContext
  text $ "Welome!" <> subject

corsHeader :: ActionCtxT a (WebStateM SqlBackend () ()) a
corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

initHook :: ApiAction () (HVect '[])
initHook = return HNil

errorHandler :: MonadIO m => Status -> ActionCtxT ctx m b
errorHandler status
  | status == notFound404 =
    Util.errorJson Util.NotFound
  | otherwise = do
    setStatus status
    json $ object [ "error" .= object [
                      "code" .= show status,
                      "message" .= T.decodeUtf8 (statusMessage status)
                    ]
                  ]

tokenFromHeader :: Text -> Maybe Text
tokenFromHeader = T.stripPrefix "Bearer "

extractToken :: JWT.JSON -> Maybe JWT.JWTClaimsSet
extractToken bearerToken =
  JWT.claims <$> JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken

retrieveServerToken :: (SpockConn m ~ SqlBackend, Monad m, HasSpock m) =>
                       Maybe JWT.StringOrURI -> m (Maybe (Entity Token))
retrieveServerToken (Just tokenId) = Util.runSQL $ P.selectFirst [TokenTokenId ==. Util.showText tokenId] []
retrieveServerToken Nothing        = return Nothing

validateToken :: UTCTime -> Token -> Maybe Token
validateToken currentTime token
  | tokenValidUntil token < currentTime = Just token
  | otherwise = Nothing

conditionalTokenEntityUnpack :: (Token -> Maybe Token) -> Maybe (Entity Token) -> Maybe Token
conditionalTokenEntityUnpack = flip ((>>=) . fmap entityVal)

authHook :: ApiAction (HVect xs) (HVect (Email ': xs))
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
