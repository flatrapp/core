{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Util where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import qualified Crypto.Hash.SHA512        as SHA
import           Data.Aeson                ( FromJSON
                                           , object
                                           , (.=)
                                           , eitherDecodeStrict'
                                           )
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as B16
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import qualified Data.Word8
import qualified Database.Persist          as P
import           Database.Persist          ((==.))
import           Database.Persist.Sqlite   ( SqlBackend
                                           , SqlPersistT
                                           , Key
                                           , ToBackendKey
                                           , runSqlConn
                                           , fromSqlKey
                                           )
import qualified Model.CoreTypes           as CoreT
import           Network.HTTP.Types.Status
import           Prelude                   hiding (length)
import           System.Random
import           Web.Spock

randomText :: Int -> StdGen -> T.Text
randomText len gen  = makeHex $ randomBS len gen

randomBytes :: Int -> StdGen -> [Data.Word8.Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

makeHex :: BS.ByteString -> T.Text
makeHex = E.decodeUtf8 . B16.encode

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . E.encodeUtf8

hashPassword :: T.Text -> BS.ByteString -> T.Text
hashPassword password salt =
     makeHex . SHA.finalize $ SHA.updates SHA.init [salt, E.encodeUtf8 password]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend)
       => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

data JsonError
  = CredentialsWrong
  | Unauthorized
  | BadRequest String
  | NotFound
  | EmailNotVerified
  | InvitationCodeInvalid
  | NotInvited
  | UserEmailExists
  | InvitationEmailExists
  | UserDisabled
  deriving (Show)


errorJson :: Control.Monad.IO.Class.MonadIO m =>
             JsonError -> ActionCtxT ctx m b
errorJson err = do
  setStatus status
  json $
    object
    [ "error" .= object [
        "code" .= code,
        "message" .= msg
      ]
    ]
  where
    (code, msg) = (T.pack *** T.pack) strs
    (status, strs) = conv' err

    conv' :: JsonError -> (Status, (String, String))
    conv' CredentialsWrong      = (unauthorized401, ("credentials_wrong", "User does not exist or password is wrong."))
    conv' Unauthorized          = (unauthorized401, ("unauthorized", "You are not authorized to access this resource."))
    conv' (BadRequest errorMsg) = (badRequest400,   ("bad_request", errorMsg))
    conv' NotFound              = (notFound404,     ("not_found", "The requested resource could not be found."))
    conv' EmailNotVerified      = (forbidden403,    ("email_not_verified", "You have not verified your email address yet."))
    conv' InvitationCodeInvalid = (forbidden403,    ("invitation_code_invalid", "This is not a valid invitation code."))
    conv' NotInvited            = (forbidden403,    ("not_invited", "Your email address is not invited."))
    conv' UserEmailExists       = (conflict409,     ("user_email_exists", "A user with this email address already exists."))
    conv' InvitationEmailExists = (conflict409,     ("invitation_email_exists", "An invitation with this email address already exists."))
    conv' UserDisabled          = (forbidden403,    ("user_disabled", "This user is disabled and has to be enabled before being able to log in."))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple Nothing _         = Nothing
maybeTuple _ Nothing         = Nothing
maybeTuple (Just a) (Just b) = Just (a, b)

integerKey :: (Num n, ToBackendKey SqlBackend record) => Key record -> n
integerKey = fromIntegral . fromSqlKey

showText :: (Show a) => a -> T.Text
showText = T.pack . show

emptyResponse :: CoreT.ApiAction ctx a
emptyResponse = do
  setStatus noContent204
  bytes BS.empty

eitherJsonBody :: (FromJSON a) => CoreT.ApiAction ctx a
eitherJsonBody = do
  b <- body
  case eitherDecodeStrict' b of  -- TODO mapLeft
    -- TODO DO NOT expose literal errors to the client
    -- it might include sensitive application details.
    -- Might require a change to the Aeson library.
    Left err ->
      errorJson . BadRequest $ "Failed to parse json: " ++ err
    Right val ->
      return val

-- TODO combine get and selectFirst or think about why each is necessary
-- TODO figure out a better type signature for all trySql methods
trySqlGet :: (P.PersistEntityBackend b ~ SqlBackend
             , SpockConn (ActionCtxT ctx m) ~ SqlBackend, MonadIO m
             , P.PersistEntity b, HasSpock (ActionCtxT ctx m)
             ) => Key b -> ActionCtxT ctx m b
trySqlGet entityId = do
  mEntity <- runSQL $ P.get entityId
  case mEntity of
    Nothing -> errorJson NotFound
    Just entity -> return entity

-- strict version which crashes if it's not there
trySqlGet' :: ( P.PersistEntityBackend b ~ SqlBackend, SpockConn m ~ SqlBackend
              , P.PersistEntity b, HasSpock m, Monad m
              ) => Key b -> m b
trySqlGet' entityId = do
  mEntity <- runSQL $ P.get entityId
  case mEntity of
    Nothing -> error "I fucked up, this really value really should be there!"
    Just entity -> return entity

trySqlSelectFirst :: ( P.PersistEntityBackend record ~ SqlBackend
                     , SpockConn (ActionCtxT ctx m) ~ SqlBackend
                     , P.PersistField typ
                     , MonadIO m
                     , P.PersistEntity record
                     , HasSpock (ActionCtxT ctx m)
                     ) => P.EntityField record typ
                       -> typ
                       -> ActionCtxT ctx m (P.Entity record)
trySqlSelectFirst = trySqlSelectFirstError NotFound

-- strict version which crashes if it's not there
trySqlSelectFirst' :: ( P.PersistEntityBackend record ~ SqlBackend
                      , SpockConn m ~ SqlBackend , P.PersistField typ
                      , P.PersistEntity record, HasSpock m, Monad m
                      ) => P.EntityField record typ -> typ -> m (P.Entity record)
trySqlSelectFirst' identifier entityId = do
  mEntity <- runSQL $ P.selectFirst [identifier ==. entityId] []
  case mEntity of
    Nothing -> error "I fucked up, this really value really should be there!"
    Just entity -> return entity

trySqlSelectFirstError :: ( P.PersistEntityBackend record ~ SqlBackend
                          , SpockConn (ActionCtxT ctx m) ~ SqlBackend
                          , P.PersistField typ
                          , MonadIO m
                          , P.PersistEntity record
                          , HasSpock (ActionCtxT ctx m)
                          ) => JsonError
                            -> P.EntityField record typ
                            -> typ
                            -> ActionCtxT ctx m (P.Entity record)
trySqlSelectFirstError errStatus identifier entityId = do
  mEntity <- runSQL $ P.selectFirst [identifier ==. entityId] []
  case mEntity of
    Nothing -> errorJson errStatus
    Just entity -> return entity
