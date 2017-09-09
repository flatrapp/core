{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Network.HTTP.Types.Status
import           Web.Spock
import           Web.Spock.Config

import           Data.Time.Clock.POSIX     (getPOSIXTime)

import           Control.Monad.IO.Class
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Data.Aeson                hiding (json)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import           Database.Persist.Sqlite   hiding (delete, get)
import           GHC.Generics
import           Prelude                   hiding (exp)
import qualified Prelude                   as P

import qualified Web.JWT                   as JWT

import           Model.CoreTypes
import           Model.JsonTypes
import qualified Util                      as Util

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

mySpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
mySpockCfg sess conn st =
  do defSess <- defaultSessionCfg sess
     return
       SpockCfg
       { spc_initialState = st
       , spc_database = conn
       , spc_sessionCfg = defSess
       , spc_maxRequestSize = Just (5 * 1024 * 1024)
       , spc_errorHandler = errorHandler
       , spc_csrfProtection = False
       , spc_csrfHeaderName = "X-Csrf-Token"
       , spc_csrfPostName = "__csrf_token"
       }

errorHandler :: Status -> ActionCtxT () IO ()
errorHandler notFound404 = do
    setStatus notFound404
    text "Sorry, nothing found :/"
errorHandler s = do
    setStatus s
    text $ (T.pack . show $ (statusCode s)) <> " - " <> T.decodeUtf8 (statusMessage s)

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- mySpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8124 (spock spockCfg app)

corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

jwtSecret = "secret"

app :: Api
app =
  prehook corsHeader $ do
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople
  get ("people" <//> var) $ \personId -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a person with matching id"
      Just thePerson -> json thePerson
  delete ("people" <//> var) $ \(personId :: PersonId) -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a person with matching id"
      Just thePerson -> do
        runSQL $ P.delete personId
        text $ "Thanks for deleting the user"
  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        setStatus created201
        newId <- runSQL $ insert thePerson
        json $ object ["result" .= String "success", "id" .= newId]
  options "auth" $ do
    setHeader "Access-Control-Allow-Headers" "Content-Type"
  get "restricted" $ do
    maybeBearerToken <- header "Authorization"
    case maybeBearerToken of
        Nothing -> do
          setStatus unauthorized401
          errorJson 3 "Please authorize yourself"
        Just bearerToken -> do
            let maybeSignature = JWT.decodeAndVerifySignature (JWT.secret jwtSecret) bearerToken
            case maybeSignature of
                Nothing -> do
                  setStatus forbidden403
                  errorJson 4 "Nope, Chuck Testa"
                Just signature -> do
                  textStringShow signature
                  --text $ "Welome!"
  post "auth" $ do
    maybeLogin <- jsonBody :: ApiAction (Maybe LoginCredentials)
    case maybeLogin of
      Nothing -> do
        setStatus badRequest400
        errorJson 3 "Invalid request"
      Just loginCredentials -> do
        currentTime <- liftIO getPOSIXTime
        let validFor = tokenTimeout + tokenGracePeriod
        tokenId <- liftIO $ Util.randomText 64
        let key = JWT.secret jwtSecret
        let cs = JWT.def { JWT.exp = JWT.numericDate $ currentTime + validFor
                         , JWT.jti = JWT.stringOrURI tokenId
                         , JWT.unregisteredClaims = Map.fromList [("email", String $ email loginCredentials)]
                         }
        let jwt = JWT.encodeSigned JWT.HS256 key cs
        json $ object [ "token"    .= jwt
                      , "tokenId"  .= tokenId
                      , "validFor" .= validFor
                      ]
  get "test" $ do
    currentTime <- liftIO $ (getPOSIXTime)
    text $ pack $ (show currentTime) <> (show $ currentTime + tokenTimeout + tokenGracePeriod)

textStringShow = text . pack . show

options = hookRoute OPTIONS

tokenTimeout = 60 * 60
tokenGracePeriod = 60

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "error" .= object ["code" .= code, "message" .= message]
    ]
