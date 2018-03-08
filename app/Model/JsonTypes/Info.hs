{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Info (Info(..)) where

import           Data.Aeson      (ToJSON)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

data Info =
     Info { version     :: Text
          , currentTime :: UTCTime
          , name        :: Text
          } deriving (Show, Generic)

instance ToJSON Info
