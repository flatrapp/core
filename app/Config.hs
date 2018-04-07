{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}

module Config
    ( FlatrCfg(..)
    , SmtpConfig(..)
    , parseConfig
    )
where

import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TL
import           Dhall             (Interpret, Generic, input, auto)

data FlatrCfg
   = FlatrCfg
   { db               :: T.Text
   , port             :: Integer
   , jwtSecret        :: T.Text
   , whitelistedMails :: [T.Text]
   , smtpConfig       :: Maybe SmtpConfig
   } deriving (Generic)
instance Interpret FlatrCfg

data SmtpConfig
   = SmtpConfig
   { host     :: T.Text
   , smtpPort :: Integer
   , username :: T.Text
   , password :: T.Text
   , sender   :: T.Text
   } deriving (Generic)
instance Interpret SmtpConfig

parseConfig :: FilePath -> IO FlatrCfg
parseConfig cfgFile = do
  dhall <- TL.readFile cfgFile
  input auto dhall
