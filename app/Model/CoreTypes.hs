{-# LANGUAGE OverloadedStrings          #-}

module Model.CoreTypes
    ( Api
    , ApiAction
    , Email
    , ApiState(..)
    , SqlQuery
    )
where

import           Control.Monad.Logger    (LoggingT)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT)
import           Web.Spock               (SpockCtxM, SpockActionCtx)

import           Config                  (FlatrCfg)

type Api ctx = SpockCtxM ctx SqlBackend () ApiState ()
type ApiAction ctx a = SpockActionCtx ctx SqlBackend () ApiState a
type SqlQuery a = SqlPersistT (LoggingT IO) a
-- TODO create a type that forces authentication with
-- ListContains n Email xs =>

type Email = Text

newtype ApiState
      = ApiState
      { apiCfg :: FlatrCfg
      }
