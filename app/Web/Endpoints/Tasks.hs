{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Tasks where

import           Control.Monad.IO.Class
import           Data.Aeson                   hiding (json)
import qualified Data.Text                    as T
import           Data.Time.Clock
import           Database.Persist             hiding (delete, get)
import qualified Database.Persist             as P
import           Network.HTTP.Types.Status
import           System.Random
import           Web.Spock

import qualified Model.CoreTypes              as SqlT
import qualified Model.JsonTypes.Registration as JsonRegistration
import qualified Model.JsonTypes.Task         as JsonTask
import qualified Model.JsonTypes.Turn         as JsonTurn
import qualified Model.JsonTypes.User         as JsonUser
import           Util                         (errorJson, runSQL)
import qualified Util

routeTasks = do
  get "tasks" $ do
    allTasks <- runSQL $ selectList [] [Asc SqlT.TaskId]
    json $ map JsonTask.jsonTask allTasks
  post "tasks" $ do
    maybeTask <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonTask.Task)
    case maybeTask of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest
      Just task -> do
        setStatus created201
        -- post new Turn
        currentTime <- liftIO getCurrentTime
        allUsers <- runSQL $ selectKeysList [] [Asc SqlT.UserId]
        turnId <- runSQL $ insert (SqlT.Turn (Prelude.head allUsers) currentTime)
        -- post actual Task
        taskId <- runSQL $ insert SqlT.Task {
                                              SqlT.taskTitle = JsonTask.title task
                                            , SqlT.taskFrequency = JsonTask.frequency task
                                            , SqlT.taskCompletionTime = JsonTask.completionTime task
                                            , SqlT.taskNextTurn = turnId
                                            }
        maybeTask <- runSQL $ selectFirst [SqlT.TaskId ==. taskId] []
        case JsonTask.jsonTask <$> maybeTask of
          Nothing -> error "I fucked up #1"
          Just theTask -> json theTask
