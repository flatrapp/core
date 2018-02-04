{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import qualified Config
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite hiding (delete, get)
import           Model.CoreTypes
import           Web.App

main :: IO ()
main = do
  cfg <- Config.parseConfig "flatrapp.cfg"
  pool <- runStdoutLoggingT $ createSqlitePool (Config.db cfg) 5
  spockCfg <- mySpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock (Config.port cfg) (spock spockCfg (app cfg))

mySpockCfg :: sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
mySpockCfg sess conn st =
  do defSess <- defaultSessionCfg sess
     return
       SpockCfg
       { spc_initialState   = st
       , spc_database       = conn
       , spc_sessionCfg     = defSess
       , spc_maxRequestSize = Just (5 * 1024 * 1024)
       , spc_errorHandler   = errorHandler
       , spc_csrfProtection = False
       , spc_csrfHeaderName = "X-Csrf-Token"
       , spc_csrfPostName   = "__csrf_token"
       }
