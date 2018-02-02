{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Tasks where

import           Control.Monad.IO.Class
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import qualified Database.Persist.Sql      as PSql
import           Network.HTTP.Types.Status
import           Text.Printf
import           Web.Spock

import qualified Model.CoreTypes           as SqlT
import qualified Model.JsonTypes.Task      as JsonTask
import           Util                      (errorJson, runSQL)
import qualified Util

routeTasks = do
  get "tasks" $ do
    allTasks <- runSQL $ selectList [] [Asc SqlT.TaskId]
    json $ map JsonTask.jsonTask allTasks
  get ("tasks" <//> var) $ \(taskId :: SqlT.TaskId) -> do
    maybeTask <- runSQL $ P.selectFirst [SqlT.TaskId ==. taskId] []
    case JsonTask.jsonTask <$> maybeTask of
      Nothing -> do
        setStatus notFound404
        errorJson Util.TaskNotFound
      Just theTask -> json theTask  -- TODO return turns
  delete ("tasks" <//> var) $ \(taskId :: SqlT.TaskId) -> do
    maybeTask <- runSQL $ P.get taskId :: SqlT.ApiAction ctx (Maybe SqlT.Task)
    case maybeTask of
      Nothing -> do
        setStatus notFound404
        errorJson Util.TaskNotFound
      Just _theTask -> do
        runSQL $ P.delete taskId
        setStatus noContent204
        text ""  -- TODO check if empty response is possible
  -- TODO implement put "tasks" $ do
  post "tasks" $ do
    maybeTask <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonTask.Task)
    case maybeTask of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest
      Just task -> do
        -- post actual Task
        taskId <- runSQL $ insert SqlT.Task {
              SqlT.taskTitle          = JsonTask.title task
            , SqlT.taskFrequency      = JsonTask.frequency task
            , SqlT.taskCompletionTime = JsonTask.completionTime task
            }
        -- post new TaskUsers  TODO return in response
        currentTime <- liftIO getCurrentTime
        let users = JsonTask.users task
        let insertIt userId = runSQL $ insert SqlT.TaskUser {
              SqlT.taskUserTaskId = taskId
            , SqlT.taskUserUserId = PSql.toSqlKey . fromInteger $ userId
            }
        _ <- mapM insertIt users -- TODO check return value
        -- post initial Turn TODO return in response
        turnId <- runSQL $ insert SqlT.Turn {
              SqlT.turnUserId = PSql.toSqlKey . fromInteger . Prelude.head $ users
            , SqlT.turnTaskId = taskId
            , SqlT.turnDate   = currentTime  -- TODO something real
            }
        maybeTask <- runSQL $ selectFirst [SqlT.TaskId ==. taskId] []
        case JsonTask.jsonTask <$> maybeTask of
          Nothing -> error "I fucked up #1"
          Just theTask -> do
            setStatus created201
            let location :: T.Text = T.pack $ printf "/tasks/%d" (Util.integerKey taskId :: Integer)
            setHeader "Location" location
            json theTask
