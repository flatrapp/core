{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import qualified Model.CoreTypes        as SqlT
import           Model.JsonTypes.Info
import           Web.Spock

apiVersion :: Text
apiVersion = "v0.1"

serverName :: Text
serverName = "core"

getInfoAction :: SqlT.ApiAction ctx a
getInfoAction = do
  currentTime' <- liftIO getCurrentTime
  json Info { version     = apiVersion
            , currentTime = currentTime'
            , name        = serverName
            }

routeInfo :: SqlT.Api ctx
routeInfo = get "info" getInfoAction
