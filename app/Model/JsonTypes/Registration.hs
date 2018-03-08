{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.JsonTypes.Registration (Registration(..)) where

import           Control.Monad (when)
import           Data.Aeson    (withObject, parseJSON, FromJSON)
import           Data.Text     (Text)
import           Data.Maybe    (isNothing, isJust)
import           GHC.Generics  (Generic)

import           Model.JsonTypes.Parsing

data Registration =
    Registration { email          :: Maybe Text
                 , firstName      :: Text
                 , lastName       :: Text
                 , password       :: Text
                 , invitationCode :: Maybe Text
                 , absent         :: Bool
                 } deriving (Show, Generic)

instance FromJSON Registration where
  parseJSON = withObject "Registration" $ \o -> do
    email' <- parseMaybeIf validEmail "email" o
    firstName' <- parseIf nonEmptyString "firstName" o
    lastName' <- parseIf nonEmptyString "lastName" o
    password' <- parseIf nonEmptyString "password" o
    invitationCode' <- parseMaybeIf nonEmptyString "invitationCode" o
    absent' <- parseIf always "absent" o

    when (all isNothing [email', invitationCode']) $
      fail "email and invitationCode cannot both be missing"
    when (all isJust [email', invitationCode']) $
      fail "email and invitationCode cannot both be present"
    return Registration { email = email'
                        , firstName      = firstName'
                        , lastName       = lastName'
                        , password       = password'
                        , invitationCode = invitationCode'
                        , absent         = absent'
                        }
