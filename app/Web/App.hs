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
import           Data.Time.Clock           (getCurrentTime)
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

authHook :: ApiAction (HVect xs) (HVect (Email ': xs))
authHook = do
    oldCtx <- getContext
    maybeAuthHeader <- header "Authorization"
    case maybeAuthHeader of
      Nothing ->
        Util.errorJson Util.Unauthorized
      Just authHeader ->
        case T.stripPrefix "Bearer " authHeader of
          Nothing -> do
            Util.errorJson Util.Unauthorized
          Just bearerToken -> do
            let maybeClaims = JWT.claims <$>
                          JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken
            let maybeTokenId = JWT.jti =<< maybeClaims
            let maybeSubject = JWT.sub =<< maybeClaims
            case maybeTokenId `Util.maybeTuple` maybeSubject of
              Nothing ->
                Util.errorJson Util.Unauthorized  -- Token is invalid
              Just (tokenId, subject) -> do
                maybeT <- Util.runSQL $ P.selectFirst [TokenTokenId ==. Util.showText tokenId] []
                case maybeT of
                  Nothing ->
                    Util.errorJson Util.Unauthorized
                  Just (Entity _tokenId token) -> do
                    currentTime <- liftIO getCurrentTime
                    if tokenValidUntil token < currentTime then do
                      let _ = token :: Token
                      Util.errorJson Util.Unauthorized
                    else
                      return $ (pack . show $ subject) :&: oldCtx
