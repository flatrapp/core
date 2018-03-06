{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite hiding (delete, get)
import           System.Environment

import qualified Config
import           Model.SqlTypes          (migrateAll)
import           Model.CoreTypes         (ApiState(..))
import           Web.App


main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs args
  | [] <- args = runApp "flatrapp.cfg"
  | "-h" `elem` args || "--help" `elem` args = do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " [flatrapp.cfg]"
      putStrLn $ "       " ++ progName ++ " [-h]"
  | [cfg] <- args = runApp cfg
  | otherwise = do
      putStrLn "Incorrect usage"
      putStrLn ""
      parseArgs ["-h"]

runApp :: FilePath -> IO ()
runApp cfgFile = do
  cfg <- Config.parseConfig cfgFile
  pool <- runStdoutLoggingT $ createSqlitePool (Config.db cfg) 5
  spockCfg <- mySpockCfg () (PCPool pool) (ApiState cfg)
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
