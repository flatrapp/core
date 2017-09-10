{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Web.App where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                hiding (json)
import           Data.HVect                hiding (pack)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)
import           Database.Persist.Sqlite   (SqlBackend)
import           Model.CoreTypes
import           Model.JsonTypes
import           Network.HTTP.Types.Status
import qualified Util
import           Web.Endpoints.Users
import qualified Web.JWT                   as JWT
import           Web.Spock

textStringShow :: (Show a) => a -> ActionCtxT ctx (WebStateM SqlBackend () ()) a
textStringShow = text . pack . show

app :: Api ()
app =
  prehook corsHeader $
  prehook initHook $ do
    routeUsers
    prehook authHook $ do
      get "restricted" $ do
        (signature :: JWT.JWT JWT.VerifiedJWT) <- liftM findFirst getContext
        let claims  = JWT.sub $ JWT.claims signature
        textStringShow claims
        text $ "Welome!"
    post "auth" $ do
      maybeLogin <- jsonBody :: ApiAction ctx (Maybe LoginCredentials)
      case maybeLogin of
        Nothing -> do
          setStatus badRequest400
          Util.errorJson 3 "Invalid request"
        Just loginCredentials -> do
          currentTime <- liftIO getPOSIXTime
          let validFor = tokenTimeout + tokenGracePeriod
          tokenId <- liftIO $ Util.randomText 64
          let key = JWT.secret jwtSecret
          let cs = JWT.def { JWT.exp = JWT.numericDate $ currentTime + validFor
                           , JWT.jti = JWT.stringOrURI tokenId
                           , JWT.sub = JWT.stringOrURI $ email loginCredentials
                           }
          let jwt = JWT.encodeSigned JWT.HS256 key cs
          json $ object [ "token"    .= jwt
                        , "tokenId"  .= tokenId
                        , "validFor" .= validFor
                        ]
    get "test" $ do
      currentTime <- liftIO getPOSIXTime
      text $ pack $ show currentTime <> (show $ currentTime + tokenTimeout + tokenGracePeriod)
    -- Allow for pre-flight AJAX requests
    hookAny OPTIONS $ \_ ->
      setHeader "Access-Control-Allow-Headers" "Content-Type"

tokenTimeout :: Data.Time.Clock.POSIX.POSIXTime
tokenTimeout = 60 * 60

tokenGracePeriod :: Data.Time.Clock.POSIX.POSIXTime
tokenGracePeriod = 60

corsHeader :: ActionCtxT a (WebStateM SqlBackend () ()) a
corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

initHook :: ApiAction () (HVect '[])
initHook = return HNil

jwtSecret :: Text
jwtSecret = "secret"

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

authHook :: ApiAction (HVect xs) (HVect ((JWT.JWT JWT.VerifiedJWT) ': xs))
authHook = do
    oldCtx <- getContext
    maybeBearerToken <- header "Authorization"
    case maybeBearerToken of
      Nothing -> do
        setStatus unauthorized401
        Util.errorJson 3 "Please authorize yourself"
      Just bearerToken -> do
        let maybeToken = JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken
        case maybeToken of
          Nothing -> do
            setStatus forbidden403
            Util.errorJson 4 "You are not allowed here"
          Just token -> do
            return (token :&: oldCtx)
