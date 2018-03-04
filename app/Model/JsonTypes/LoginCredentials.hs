{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.JsonTypes.LoginCredentials where

import           Data.Aeson              (withObject, parseJSON, FromJSON)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Model.JsonTypes.Parsing

data LoginCredentials =
    LoginCredentials { email    :: Text
                     , password :: Text
                     } deriving (Show, Generic)

instance FromJSON LoginCredentials where
  parseJSON = withObject "Invitation" $ \o -> do
    email' <- parseIf nonEmptyString "email" o
    password' <- parseIf nonEmptyString "password" o
    return LoginCredentials { email    = email'
                            , password = password'
                            }
