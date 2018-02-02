{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Task where

import           Data.Aeson           hiding (json)
import           Data.Text            (Text)
import           Database.Persist.Sql
import           GHC.Generics
import qualified Model.CoreTypes      as SqlT
import           Prelude              hiding (id)
import qualified Util

data Task =
    Task { id             :: Maybe Integer
         , title          :: Text
         , frequency      :: Int
         , completionTime :: Int
         , users          :: [Integer]
         , nextTurn       :: Integer
         } deriving (Show, Generic)

instance ToJSON Task
instance FromJSON Task


jsonTask :: Entity SqlT.Task -> Task
jsonTask (Entity taskId task) =
    Task { id        = Just $ Util.integerKey taskId
         , title     = SqlT.taskTitle task
         , frequency = SqlT.taskFrequency task
         , completionTime = SqlT.taskCompletionTime task
         , users     = []
         , nextTurn  = 1 -- SqlT.taskNextTurn task
         }
