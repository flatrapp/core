{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Task where

import           Data.Aeson           (ToJSON)
import           Data.Text            (Text)
import           Database.Persist.Sql (Entity(..), Key(..))
import           GHC.Generics         (Generic)
import qualified Model.SqlTypes       as SqlT
import           Prelude              hiding (id)
import           Model.JsonTypes.Turn (Turn)
import qualified Util

data Task =
    Task { id             :: Integer
         , title          :: Text
         , description    :: Text
         , frequency      :: Int
         , completionTime :: Int
         , users          :: [Integer]
         , currentTurn    :: Maybe Turn
         , upcomingTurns  :: [Turn]
         } deriving (Show, Generic)

instance ToJSON Task

jsonTask :: [Key SqlT.User] -> (Maybe Turn, [Turn]) -> Entity SqlT.Task -> Task
jsonTask users' (currentTurn', upcomingTurns') (Entity taskId task) =
    Task { id             = Util.integerKey taskId
         , title          = SqlT.taskTitle task
         , description    = SqlT.taskDescription task
         , frequency      = SqlT.taskFrequency task
         , completionTime = SqlT.taskCompletionTime task
         , users          = map Util.integerKey users'
         , currentTurn    = currentTurn'
         , upcomingTurns  = upcomingTurns'
         }
