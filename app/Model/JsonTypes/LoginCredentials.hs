{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.LoginCredentials where

import           Data.Aeson   hiding (json)
import           Data.Text    (Text)
import           GHC.Generics

data LoginCredentials =
    LoginCredentials { email    :: Text
                     , password :: Text
                     } deriving (Show, Generic)

instance FromJSON LoginCredentials
instance ToJSON LoginCredentials
