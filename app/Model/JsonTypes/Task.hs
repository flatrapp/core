{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Task where

import           Data.Aeson           hiding (json)
import           Data.Text            (Text)
import           Database.Persist.Sql
import           GHC.Generics
import qualified Model.CoreTypes      as SqlT
import           Prelude              hiding (id)
import           Model.JsonTypes.Turn
import qualified Util

data Task =
    Task { id             :: Maybe Integer
         , title          :: Text
         , frequency      :: Int
         , completionTime :: Int
         , users          :: [Integer]
         , currentTurn    :: Maybe Turn
         , upcomingTurns  :: [Turn]
         } deriving (Show, Generic)

instance ToJSON Task
instance FromJSON Task


jsonTask :: [Key SqlT.User] -> (Maybe Turn, [Turn]) -> Entity SqlT.Task -> Task
jsonTask users (currentTurn', upcomingTurns') (Entity taskId task) =
    Task { id             = Just $ Util.integerKey taskId
         , title          = SqlT.taskTitle task
         , frequency      = SqlT.taskFrequency task
         , completionTime = SqlT.taskCompletionTime task
         , users          = map Util.integerKey users
         , currentTurn    = currentTurn'
         , upcomingTurns  = upcomingTurns'
         }
