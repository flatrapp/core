{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Info where

import           Data.Aeson      hiding (json)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

data Info =
     Info { version     :: Text
          , currentTime :: UTCTime
          } deriving (Show, Generic)

instance ToJSON Info
