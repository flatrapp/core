{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info (routeInfo) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import           Web.Spock

import           Model.CoreTypes        (ApiAction, Api)
import           Model.JsonTypes.Info   (Info(..))

apiVersion :: Text
apiVersion = "v0.1"

serverName :: Text
serverName = "core"

routeInfo :: Api ctx
routeInfo =
  get "info" getInfoAction

getInfoAction :: ApiAction ctx a
getInfoAction = do
  currentTime' <- liftIO getCurrentTime
  json Info { version     = apiVersion
            , currentTime = currentTime'
            , name        = serverName
            }
