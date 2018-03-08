{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
    ( FlatrCfg(..)
    , SmtpConfig(..)
    , parseConfig
    )
where

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text               as T

data FlatrCfg
   = FlatrCfg
   { db               :: T.Text
   , port             :: Int
   , jwtSecret        :: T.Text
   , whitelistedMails :: [T.Text]
   , smtpConfig       :: Maybe SmtpConfig
   }

data SmtpConfig
   = SmtpConfig
   { host     :: T.Text
   , smtpPort :: Integer
   , username :: T.Text
   , password :: T.Text
   , sender   :: T.Text
   }

parseConfig :: FilePath -> IO FlatrCfg
parseConfig cfgFile = do
  cfg               <- C.load [C.Required cfgFile]
  db'               <- C.require cfg "db"
  port'             <- C.require cfg "port"
  jwtSecret'        <- C.require cfg "jwtSecret"
  whitelistedMails' <- C.require cfg "whitelistedMails"
  smtpConfig'       <- lookupSmtpConfig cfg
  return $ FlatrCfg db' port' jwtSecret' whitelistedMails' smtpConfig'

lookupSmtpConfig :: C.Config -> IO (Maybe SmtpConfig)
lookupSmtpConfig cfg = do
  host'     <- C.lookup cfg "smtpConfig.host"
  smtpPort' <- C.lookup cfg "smtpConfig.smtpPort"
  username' <- C.lookup cfg "smtpConfig.username"
  password' <- C.lookup cfg "smtpConfig.password"
  sender'   <- C.lookup cfg "smtpConfig.sender"
  return $ applyJust SmtpConfig host' smtpPort' username' password' sender'

-- TODO do something better, maybe Template Haskell if it's the only solution
applyJust :: (a -> b -> c -> d -> e-> result)
           -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e
           -> Maybe result
applyJust f (Just a) (Just b) (Just c) (Just d) (Just e)= Just $ f a b c d e
applyJust _ _ _ _ _ _ = Nothing
