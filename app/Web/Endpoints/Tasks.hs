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
import           Network.HTTP.Types.Status (created201)
import           Text.Printf
import           Web.Spock                 hiding (head)

import           Model.CoreTypes           (ApiAction, Api)
import qualified Model.SqlTypes            as SqlT
import qualified Model.JsonTypes.Task      as JsonTask
import qualified Model.JsonTypes.Turn      as JsonTurn
import           Util                      (runSQL)
import qualified Util

-- TODO restrict all endpoints to logged in users
routeTasks :: Api ctx
routeTasks = do
  get "tasks" getTasksAction
  get ("tasks" <//> var) $ \taskId ->  -- TODO use Kleisli combinator >=>
    Util.trySqlSelectFirst SqlT.TaskId taskId >>= getTaskAction
  delete ("tasks" <//> var) $ \taskId ->
    Util.trySqlGet taskId >> deleteTaskAction taskId
  post ("tasks" <//> var <//> "finish") $ \taskId ->
    Util.trySqlSelectFirst SqlT.TurnTaskId taskId >> finishTaskAction taskId
  put ("tasks" <//> var) $ \taskId ->
    Util.eitherJsonBody >>= putTaskAction taskId
  post "tasks" $ Util.eitherJsonBody >>= postTasksAction

getTaskInfo :: (JsonTask.Task -> ApiAction ctx a) -> Entity SqlT.Task -> ApiAction ctx a
getTaskInfo fun theTask@(Entity taskId _task) = do
  users <- map (\(Entity _ taskUser) -> SqlT.taskUserUserId taskUser)
             <$> runSQL (P.selectList [SqlT.TaskUserTaskId ==. taskId] [])
  turns <- map JsonTurn.jsonTurn
             <$> runSQL (P.selectList [SqlT.TurnTaskId ==. taskId, SqlT.TurnFinishedAt ==. Nothing] [Asc SqlT.TurnStartDate])
  currentTime <- liftIO getCurrentTime
  let splitTurns = case turns of
                     [] -> (Nothing, [])
                     (nextTurn:otherTurns) ->
                       if currentTime > JsonTurn.startDate nextTurn  then
                         (Just nextTurn, otherTurns)
                       else
                         (Nothing, otherTurns)
  fun $ JsonTask.jsonTask users splitTurns theTask

getTasksAction :: ApiAction ctx a
getTasksAction =
  json =<< mapM (getTaskInfo return) =<< runSQL (selectList [] [Asc SqlT.TaskId])

getTaskAction :: Entity SqlT.Task -> ApiAction ctx a
getTaskAction = getTaskInfo json

deleteTaskAction :: SqlT.TaskId -> ApiAction ctx a
deleteTaskAction taskId = do
  runSQL $ P.delete taskId
  Util.emptyResponse

finishTaskAction :: SqlT.TaskId -> ApiAction ctx a
finishTaskAction taskId = do
  currentTime <- liftIO getCurrentTime
  runSQL $ P.updateWhere
    [ SqlT.TurnTaskId ==. taskId
    , SqlT.TurnFinishedAt ==. Nothing
    , SqlT.TurnStartDate <. currentTime
    ]
    [SqlT.TurnFinishedAt =. Just currentTime]
  Util.emptyResponse

putTaskAction :: SqlT.TaskId -> JsonTask.Task -> ApiAction ctx a
-- TODO combine with postTaskAction
-- TODO maybe to JsonTask.Task with argument pattern matching as well
putTaskAction taskId task = do
  _task <- Util.trySqlGet taskId  -- we just want to check if it is there TODO maybe think of something better
  runSQL $ P.replace taskId SqlT.Task {
        SqlT.taskTitle          = JsonTask.title task
      , SqlT.taskDescription    = JsonTask.description task
      , SqlT.taskFrequency      = JsonTask.frequency task
      , SqlT.taskCompletionTime = JsonTask.completionTime task  -- TODO assert than cannot be negative
      }
  returnNewTask taskId

postTasksAction :: JsonTask.Task -> ApiAction ctx a
postTasksAction task = do
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
      , SqlT.turnStartDate  = addUTCTime (1800::NominalDiffTime) currentTime  -- TODO something smart
      , SqlT.turnFinishedAt = Nothing
      }
  returnNewTask taskId

returnNewTask :: SqlT.TaskId -> ApiAction ctx a
returnNewTask taskId = do
  newTask <- Util.trySqlSelectFirst' SqlT.TaskId taskId
  setStatus created201
  let location :: T.Text = T.pack $ printf "/tasks/%d" (Util.integerKey taskId :: Integer)
  setHeader "Location" location
  getTaskInfo json newTask
