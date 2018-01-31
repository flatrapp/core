{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info where

import           Control.Monad.IO.Class
import           Data.Aeson             hiding (json)
import           Data.Text              (Text, pack)
import           Data.Time.Clock        (getCurrentTime)
import           Model.JsonTypes.Info
import           Web.Spock

apiVersion :: Text
apiVersion = pack "v0.1"

serverName :: Text
serverName = pack "core"

routeInfo =
  get "info" $ do
    currentTime <- liftIO getCurrentTime
    json Info { version     = apiVersion
              , currentTime = currentTime
              , name  = serverName
              }
