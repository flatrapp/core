{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import           Model.CoreTypes        (ApiAction, Api)
import           Model.JsonTypes.Info
import qualified Util
import           Web.Spock

apiVersion :: Text
apiVersion = "v0.1"

serverName :: Text
serverName = "core"

getInfoAction :: ApiAction ctx a
getInfoAction = do
  currentTime' <- liftIO getCurrentTime
  json Info { version     = apiVersion
            , currentTime = currentTime'
            , name        = serverName
            }

postInfoAction :: ApiAction ctx a
postInfoAction = do
  foo <- Util.eitherJsonBody
  text $ bar foo
  --Right j -> text $ bar j

routeInfo :: Api ctx
routeInfo = do
  get "info" getInfoAction
  post "info" postInfoAction
