{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Info where

import           Data.Aeson      hiding (json)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

data Info =
     Info { version     :: Text
          , currentTime :: UTCTime
          , name        :: Text
          } deriving (Show, Generic)

data Foo =
     Foo { bar        :: Text
         } deriving (Show, Generic)

instance FromJSON Foo
instance ToJSON Info
