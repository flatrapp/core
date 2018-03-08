{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.JsonTypes.InvitationIn (Invitation(..)) where

import           Data.Aeson              (withObject, parseJSON, FromJSON)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Model.JsonTypes.Parsing

newtype Invitation =
        Invitation { email          :: Text
                   } deriving (Show, Generic)

instance FromJSON Invitation where
  parseJSON = withObject "Invitation" $ \o -> do
    email' <- parseIf (matchesRegex "^.+@.+\\..+$") "email" o -- TODO better check
    return Invitation { email = email' }
