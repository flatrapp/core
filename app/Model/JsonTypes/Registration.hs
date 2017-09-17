{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Registration where

import           Data.Aeson   hiding (json)
import           Data.Text    (Text)
import           GHC.Generics

data Registration =
    Registration { email     :: Text
                 , firstName :: Text
                 , lastName  :: Text
                 , password  :: Text
                 } deriving (Show, Generic)

instance FromJSON Registration
