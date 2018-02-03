{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Util where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import qualified Crypto.Hash.SHA512      as SHA
import           Data.Aeson              hiding (json)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import qualified Data.Word8
import           Database.Persist.Sqlite
import           Prelude                 hiding (length)
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

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

data JsonError
  = InvalidRequest
  | UserPasswordWrong
  | Unauthorized
  | UserNotFound
  | TaskNotFound
  | BadRequest
  | TokenInvalid
  | NotFound
  | UserNotVerified
  | InvalidInvitationCode
  | NotInvited
  deriving (Show)


errorJson :: Control.Monad.IO.Class.MonadIO m =>
             JsonError -> ActionCtxT ctx m b
errorJson err =
  json $
    object
    [ "error" .= object [
        "code" .= fst (conv err),
        "message" .= snd (conv err)
      ]
    ]
  where
    conv :: JsonError -> (T.Text, T.Text)
    conv x = (T.pack *** T.pack) (conv' x)

    conv' :: JsonError -> (String, String)
    conv' InvalidRequest        = ("invalid_request", "Invalid request.")
    conv' UserPasswordWrong     = ("user_password_wrong", "User does not exist or password is wrong.")
    conv' Unauthorized          = ("aunauthorized", "Unauthorized.")
    conv' UserNotFound          = ("user_not_found", "No user exists with this ID.")
    conv' TaskNotFound          = ("task_not_found", "No task exists with this ID.")
    conv' BadRequest            = ("bad_request", "Bad request. Not understood.")
    conv' TokenInvalid          = ("token_invalid", "The token is invalid, you should authorize yourself again.")
    conv' NotFound              = ("not_found", "There's nothing here.")
    conv' UserNotVerified       = ("user_not_verified", "You have not verified your email address yet.")
    conv' NotInvited            = ("not_invited", "You are not invited.")

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple Nothing _         = Nothing
maybeTuple _ Nothing         = Nothing
maybeTuple (Just a) (Just b) = Just (a, b)

integerKey :: (Num n, ToBackendKey SqlBackend record) => Key record -> n
integerKey = fromIntegral . fromSqlKey
