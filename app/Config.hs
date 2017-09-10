{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Configurator as C
import qualified Data.Text         as T

data FlatrCfg
   = FlatrCfg
   { db        :: T.Text
   , port      :: Int
   , jwtSecret :: T.Text
   }

parseConfig :: FilePath -> IO FlatrCfg
parseConfig cfgFile =
    do cfg        <- C.load [C.Required cfgFile]
       db'        <- C.require cfg "db"
       port'      <- C.require cfg "port"
       jwtSecret' <- C.require cfg "jwtSecret"
       return (FlatrCfg db' port' jwtSecret')
