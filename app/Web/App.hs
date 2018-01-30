{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Web.App where

import           Control.Monad.IO.Class
import           Data.HVect                       hiding (pack)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Database.Persist                 as P
import           Database.Persist.Sql             hiding (delete, get)
import           Database.Persist.Sqlite          (SqlBackend)
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Model.CoreTypes
import qualified Web.JWT                          as JWT
import           Network.HTTP.Types.Status
import qualified Util
import           Web.Endpoints.Auth
import           Web.Endpoints.Info
import           Web.Endpoints.Users
import           Web.Spock

textStringShow :: (Show a) => a -> ActionCtxT ctx (WebStateM SqlBackend () ()) a
textStringShow = text . pack . show

app :: Api ()
app =
  prehook corsHeader $
  prehook initHook $ do
    routeUsers
    routeAuth
    routeInfo
    prehook authHook $
      get "secret" $ do
        (subject :: Text) <- fmap findFirst getContext
        textStringShow subject
        text "Welome!"
    get "test" $ do
      currentTime <- liftIO getPOSIXTime
      text $ pack $ show currentTime <> (show $ currentTime + tokenTimeout + tokenGracePeriod)
    -- Allow for pre-flight AJAX requests
    hookAny OPTIONS $ \_ ->
      setHeader "Access-Control-Allow-Headers" "Content-Type"

corsHeader :: ActionCtxT a (WebStateM SqlBackend () ()) a
corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

initHook :: ApiAction () (HVect '[])
initHook = return HNil

errorHandler :: Status -> ActionCtxT () IO ()
errorHandler status
  | status == notFound404 = do
    setStatus notFound404
    text "Sorry, nothing found :/"
  | otherwise             = do
    setStatus status
    text $ (T.pack . show $ statusCode status)
        <> " - "
        <> T.decodeUtf8 (statusMessage status)

authHook :: ApiAction (HVect xs) (HVect (Text ': xs))
authHook = do
    oldCtx <- getContext
    maybeBearerToken <- header "Authorization"
    case maybeBearerToken of
      Nothing -> do
        setStatus unauthorized401
        Util.errorJson Util.Unauthorized
      Just bearerToken -> do
        let maybeClaims = JWT.claims <$>
                      JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken
        let maybeTokenId = JWT.jti =<< maybeClaims
        let maybeSubject = JWT.sub =<< maybeClaims
        case maybeTokenId `Util.maybeTuple` maybeSubject of
          Nothing -> text "Token invalid"
          Just (tokenId, subject) -> do
            let tokenIdText = pack . show $ tokenId -- make typesafe with signature
            maybeT <- Util.runSQL $ P.selectFirst [TokenTokenId ==. tokenIdText] []
            case maybeT of
              Nothing -> text "You're not logged in"
              Just t  -> return $ (pack . show $ subject) :&: oldCtx
