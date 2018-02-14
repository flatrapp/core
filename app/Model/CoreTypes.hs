{-# LANGUAGE OverloadedStrings          #-}

module Model.CoreTypes where

import           Data.Text               (Text)
import           Web.Spock               (SpockCtxM, SpockActionCtx)
import           Database.Persist.Sqlite (SqlBackend)

type Api ctx = Web.Spock.SpockCtxM ctx SqlBackend () () ()
type ApiAction ctx a = Web.Spock.SpockActionCtx ctx SqlBackend () () a

type Email = Text
