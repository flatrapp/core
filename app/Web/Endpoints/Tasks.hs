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
import           Web.Spock                 hiding (head)

import qualified Model.CoreTypes           as SqlT
import qualified Model.JsonTypes.Task      as JsonTask
import qualified Model.JsonTypes.Turn      as JsonTurn
import           Util                      (errorJson, runSQL)
import qualified Util

getTaskInfo fun theTask@(Entity taskId _task) = do
  users <- map (\(Entity _ taskUser) -> SqlT.taskUserUserId taskUser)
             <$> runSQL (P.selectList [SqlT.TaskUserTaskId ==. taskId] [])
  turns <- map JsonTurn.jsonTurn
             <$> runSQL (P.selectList [SqlT.TurnTaskId ==. taskId, SqlT.TurnFinishedAt ==. Nothing] [Asc SqlT.TurnStartDate])
  currentTime <- liftIO getCurrentTime
  let splitTurns = if currentTime > JsonTurn.startDate (head turns)  then
                     (Just $ head turns, tail turns) -- TODO deal with empty turns
                   else
                     (Nothing, turns)
  fun $ JsonTask.jsonTask users splitTurns theTask

getTasksAction :: SqlT.ApiAction ctx a
getTasksAction =
  json =<< mapM (getTaskInfo return) =<< runSQL (selectList [] [Asc SqlT.TaskId])

getTaskAction :: Maybe (Entity SqlT.Task) -> SqlT.ApiAction ctx a
getTaskAction Nothing = do
  setStatus notFound404
  errorJson Util.TaskNotFound
getTaskAction (Just task) =
  getTaskInfo json task

deleteTaskAction :: SqlT.TaskId -> Maybe SqlT.Task -> SqlT.ApiAction ctx a
deleteTaskAction _ Nothing = do
  setStatus notFound404
  errorJson Util.TaskNotFound
deleteTaskAction taskId (Just _task) = do
  runSQL $ P.delete taskId
  setStatus noContent204
  text ""  -- TODO check if empty response is possible

finishTaskAction :: SqlT.TaskId -> Maybe (Entity SqlT.Turn) -> SqlT.ApiAction ctx a
finishTaskAction _ Nothing = do
  setStatus notFound404
  errorJson Util.TaskNotFound
finishTaskAction taskId (Just _turn) = do
  currentTime <- liftIO getCurrentTime
  runSQL $ P.updateWhere
    [ SqlT.TurnTaskId ==. taskId
    , SqlT.TurnFinishedAt ==. Nothing
    , SqlT.TurnStartDate <. currentTime
    ]
    [SqlT.TurnFinishedAt =. Just currentTime]
  setStatus noContent204
  text ""  -- TODO check if empty response is possible

postTasksAction :: Maybe JsonTask.Task -> SqlT.ApiAction ctx a
postTasksAction Nothing = do
  setStatus badRequest400
  errorJson Util.BadRequest
postTasksAction (Just task) = do
  -- post actual Task
  taskId <- runSQL $ insert SqlT.Task {
        SqlT.taskTitle          = JsonTask.title task
      , SqlT.taskDescription    = JsonTask.description task
      , SqlT.taskFrequency      = JsonTask.frequency task
      , SqlT.taskCompletionTime = JsonTask.completionTime task  -- TODO assert than cannot be negative
      }
  -- post new TaskUsers
  currentTime <- liftIO getCurrentTime
  let users = JsonTask.users task
  let insertIt userId = runSQL $ insertUnique SqlT.TaskUser {
        SqlT.taskUserTaskId = taskId
      , SqlT.taskUserUserId = PSql.toSqlKey . fromInteger $ userId
      }
  _ <- mapM insertIt users -- TODO check return value including Maybes
  -- post initial Turn
  -- TODO check if users is empty
  _turnId <- runSQL $ insert SqlT.Turn {
        SqlT.turnUserId     = PSql.toSqlKey . fromInteger . Prelude.head $ users
      , SqlT.turnTaskId     = taskId
      , SqlT.turnStartDate  = realToFrac 1 `addUTCTime` currentTime  -- TODO something smart
      , SqlT.turnFinishedAt = Nothing
      }
  maybeTask' <- runSQL $ selectFirst [SqlT.TaskId ==. taskId] []
  case maybeTask' of
    Nothing -> error "I fucked up #1"
    Just theTask -> do
      setStatus created201
      let location :: T.Text = T.pack $ printf "/tasks/%d" (Util.integerKey taskId :: Integer)
      setHeader "Location" location
      getTaskInfo json theTask

routeTasks = do
  get "tasks" getTasksAction
  get ("tasks" <//> var) $ \(taskId :: SqlT.TaskId) ->
    runSQL (P.selectFirst [SqlT.TaskId ==. taskId] []) >>= getTaskAction
  delete ("tasks" <//> var) $ \taskId ->
    runSQL (P.get taskId) >>= deleteTaskAction taskId
  post ("tasks" <//> var <//> "finish") $ \taskId ->
    runSQL (P.selectFirst [SqlT.TurnTaskId ==. taskId] [])
    >>= finishTaskAction taskId
  -- TODO implement put "tasks <//> var"
  post "tasks" $ jsonBody >>= postTasksAction
