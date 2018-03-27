{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoints.Info (routeInfo) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import           Web.Spock

import           Model.CoreTypes        (ApiAction, Api)
import           Model.JsonTypes.Info   (Info(..))

-- |The api version this server implementation promises to conform to
apiVersion :: Text
apiVersion = "v0.1"

-- |Name of the implementation of the server
-- Should be different for a different implementation.
serverName :: Text
serverName = "core"

routeInfo :: Api ctx
routeInfo =
  get "info" getInfoAction

-- |Get general information about the server
-- Useful for when a client is capable of talking to multiple backends
-- or for debugging purposes.
getInfoAction :: ApiAction ctx a
getInfoAction = do
  currentTime' <- liftIO getCurrentTime
  json Info { version     = apiVersion
            , currentTime = currentTime'
            , name        = serverName
            }
