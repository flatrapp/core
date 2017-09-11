{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.User where

import           Data.Aeson   hiding (json)
import           Data.Text    (Text)
import           GHC.Generics

data User =
    User { id        :: Integer
         , email     :: Text
         , firstName :: Text
         , lastName  :: Text
         } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User
