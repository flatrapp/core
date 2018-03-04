{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Model.JsonTypes.TaskIn where

import           Data.Aeson             (withObject, parseJSON, FromJSON)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Model.JsonTypes.Parsing

data Task =
    Task { title          :: Text
         , description    :: Text
         , frequency      :: Int
         , completionTime :: Int
         , users          :: [Integer]
         } deriving (Show, Generic)

instance FromJSON Task where
  parseJSON = withObject "Task" $ \o -> do
    title'          <- parseIf nonEmptyString "title" o
    frequency'      <- parseIf nonZeroPositive  "frequency" o
    description'    <- parseIf nonEmptyString "description" o
    completionTime' <- parseIf nonZeroPositive "completionTime" o
    users'          <- parseIf nonEmptyList "users" o

    return Task { title          = title'
                , description    = description'
                , frequency      = frequency'
                , completionTime = completionTime'
                , users          = users'
                }
