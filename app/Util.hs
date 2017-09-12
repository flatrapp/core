{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Monad
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Aeson              hiding (json)
import           Data.Text               (Text, pack)
import           Database.Persist.Sqlite
import           Model.CoreTypes
import           Prelude                 hiding (length)
import           System.Random
import           Web.Spock

randomText :: Int-> IO Text
randomText length = liftM (pack . take length . randomRs ('a','z')) newStdGen

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
maybeToEither e Nothing = Left e
maybeToEither e (Just value) = Right value
--maybeToEither = flip maybe Right . Left

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple Nothing _ = Nothing
maybeTuple _ Nothing = Nothing
maybeTuple (Just a) (Just b) = Just (a, b)
