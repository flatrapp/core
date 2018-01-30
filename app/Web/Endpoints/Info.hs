{-# LANGUAGE OverloadedStrings     #-}

module Web.Endpoints.Info where

import           Control.Monad.IO.Class
import           Data.Text              (Text, pack)
import           Data.Aeson             hiding (json)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Web.Spock

apiVersion :: Text
apiVersion = pack "v0.1"

routeInfo =
  get "info" $ do
    currentTime <- liftIO getCurrentTime
    json $ object [ "version" .= apiVersion
                  , "currentTime" .= formatISO8601Millis currentTime
                  ]
