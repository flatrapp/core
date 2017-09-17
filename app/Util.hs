{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Monad
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Aeson              hiding (json)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import           Data.Text               (Text, pack)
import qualified Data.Text.Encoding      as T
import           Data.Word8
import           Database.Persist.Sqlite
import           Model.CoreTypes
import           Prelude                 hiding (length)
import           System.Random
import           Web.Spock

randomText :: Int -> StdGen -> Text
randomText len gen  = makeHex $ randomBS len gen

randomBytes :: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

makeHex :: BS.ByteString -> Text
makeHex = T.decodeUtf8 . B16.encode

decodeHex :: Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ctx a
errorJson code message =
  json $
    object
    [ "error" .= object ["code" .= code, "message" .= message]
    ]

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing      = Left e
maybeToEither e (Just value) = Right value
--maybeToEither = flip maybe Right . Left

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple Nothing _         = Nothing
maybeTuple _ Nothing         = Nothing
maybeTuple (Just a) (Just b) = Just (a, b)
