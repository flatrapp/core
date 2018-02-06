{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import           Model.JsonTypes.Info
import           Web.Spock

apiVersion :: Text
apiVersion = "v0.1"

serverName :: Text
serverName = "core"

routeInfo =
  get "info" $ do
    currentTime' <- liftIO getCurrentTime
    json Info { version     = apiVersion
              , currentTime = currentTime'
              , name  = serverName
              }
