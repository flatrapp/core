{-# LANGUAGE OverloadedStrings          #-}

module Model.CoreTypes where

import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock               (SpockCtxM, SpockActionCtx)

import           Config                  (FlatrCfg)

type Api ctx = SpockCtxM ctx SqlBackend () ApiState ()
type ApiAction ctx a = SpockActionCtx ctx SqlBackend () ApiState a
-- TODO create a type that forces authentication with
-- ListContains n Email xs =>

type Email = Text

newtype ApiState
      = ApiState
      { apiCfg :: FlatrCfg
      }
