{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Tasks (routeTasks) where

import           Control.Monad             ((<=<))
import           Control.Monad.IO.Class    (liftIO)
import           Data.HVect                (HVect, ListContains)
import           Data.Maybe                (fromJust, listToMaybe, fromMaybe)
import           Data.List                 ((\\))
import           Data.Time.Clock           ( NominalDiffTime
                                           , addUTCTime, getCurrentTime)
import qualified Database.Esqueleto        as E
import           Database.Persist          hiding (delete, get)  -- TODO specify
import qualified Database.Persist          as P
import qualified Database.Persist.Sql      as PSql
import           Formatting                ((%), int, sformat)
import           Network.HTTP.Types.Status (created201)
import           Web.Spock                 hiding (head)

import           Model.CoreTypes           (ApiAction, Api, Email)
import qualified Model.SqlTypes            as SqlT
import qualified Model.JsonTypes.Task      as JsonTask
import qualified Model.JsonTypes.TaskIn    as JsonTaskIn
import qualified Model.JsonTypes.Turn      as JsonTurn
import           Util                      (eitherJsonBody, emptyResponse)
import           Query.Util                ( integerKey, runSQL, trySqlGet
                                           , trySqlSelectFirst, trySqlSelectFirst'
                                           )
import           Web.Auth                  (authHook)

routeTasks :: Api (HVect xs)
routeTasks =
  prehook authHook $ do
    get "tasks" getTasksAction
    get ("tasks" <//> var) $
      getTaskAction <=< trySqlSelectFirst SqlT.TaskId
    delete ("tasks" <//> var) $ \taskId ->
      trySqlGet taskId >> deleteTaskAction taskId
    post ("tasks" <//> var <//> "finish") $ \taskId ->
      trySqlSelectFirst SqlT.TurnTaskId taskId >> finishTaskAction taskId
    post ("tasks" <//> "update") updateTurnsAction
    put ("tasks" <//> var) $ \taskId ->
      eitherJsonBody >>= putTaskAction taskId
    post "tasks" $ eitherJsonBody >>= postTasksAction

-- |Helper function to get information about a task and its turns
getTaskInfo :: (JsonTask.Task -> ApiAction ctx a)
             -> Entity SqlT.Task
             -> ApiAction ctx a
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

getTasksAction :: ListContains n Email xs => ApiAction (HVect xs) a
getTasksAction =
  json =<< mapM (getTaskInfo return) =<< runSQL (selectList [] [Asc SqlT.TaskId])

getTaskAction :: ListContains n Email xs
              => Entity SqlT.Task -> ApiAction (HVect xs) a
getTaskAction = getTaskInfo json

deleteTaskAction :: ListContains n Email xs
                 => SqlT.TaskId -> ApiAction (HVect xs) a
deleteTaskAction taskId = do
  runSQL $ P.delete taskId
  emptyResponse

finishTaskAction :: ListContains n Email xs
                 => SqlT.TaskId -> ApiAction (HVect xs) a
finishTaskAction taskId = do
  currentTime <- liftIO getCurrentTime
  runSQL $ P.updateWhere
    [ SqlT.TurnTaskId ==. taskId
    , SqlT.TurnFinishedAt ==. Nothing
    , SqlT.TurnStartDate <. currentTime
    ]
    [SqlT.TurnFinishedAt =. Just currentTime]
  emptyResponse

putTaskAction :: ListContains n Email xs
              => SqlT.TaskId -> JsonTaskIn.Task -> ApiAction (HVect xs) a
-- TODO combine with postTaskAction
-- TODO maybe to JsonTaskIn.Task with argument pattern matching as well
putTaskAction taskId task = do
  _task <- trySqlGet taskId  -- we just want to check if it is there TODO maybe think of something better
  runSQL $ P.replace taskId SqlT.Task {
        SqlT.taskTitle          = JsonTaskIn.title task
      , SqlT.taskDescription    = JsonTaskIn.description task
      , SqlT.taskFrequency      = JsonTaskIn.frequency task
      , SqlT.taskCompletionTime = JsonTaskIn.completionTime task
      }
  returnNewTask taskId

postTasksAction :: ListContains n Email xs
                => JsonTaskIn.Task -> ApiAction (HVect xs) a
postTasksAction task = do
  -- post actual Task
  taskId <- runSQL $ insert SqlT.Task {
        SqlT.taskTitle          = JsonTaskIn.title task
      , SqlT.taskDescription    = JsonTaskIn.description task
      , SqlT.taskFrequency      = JsonTaskIn.frequency task
      , SqlT.taskCompletionTime = JsonTaskIn.completionTime task
      }
  -- post new TaskUsers
  let users = JsonTaskIn.users task  -- non emptiness is assured by JSON parsing
  let insertIt userId = runSQL $ insertUnique SqlT.TaskUser {  -- TODO check if user exists
        SqlT.taskUserTaskId = taskId
      , SqlT.taskUserUserId = PSql.toSqlKey . fromInteger $ userId
      }
  _ <- mapM insertIt users -- TODO check return value including Maybes
  updateTurns
  returnNewTask taskId

updateTurnsAction :: ListContains n Email xs => ApiAction (HVect xs) a
updateTurnsAction = do
  updateTurns
  emptyResponse

updateTurns :: ApiAction ctx ()
updateTurns = do
  users <- runSQL $ P.selectList [ SqlT.UserDisabled ==. False ] []
  let userIds = map (\(Entity key _val) -> key) users
  tasks :: [Entity SqlT.Task] <- runSQL $ P.selectList [] []
  mapM_ (updateTaskTurns userIds) tasks

-- |If this endpoint is called it will check if a Task is in need of new turns
-- and will create those. This is the case if a Task has no unfinished turns.
updateTaskTurns :: [Key SqlT.User] -> Entity SqlT.Task -> ApiAction ctx a
updateTaskTurns users (Entity taskId task) = do
  currentTime <- liftIO getCurrentTime
  unfinishedTurns <-
    runSQL $ P.selectList [ SqlT.TurnTaskId ==. taskId
                          , SqlT.TurnFinishedAt ==. Nothing
                          ] []
  finishedTurns <-
    runSQL $ P.selectList [ SqlT.TurnTaskId ==. taskId
                          , SqlT.TurnFinishedAt !=. Nothing
                          ]
                          [ Asc SqlT.TurnStartDate ]
  taskUsersValue :: [E.Value (Key SqlT.User)] <-
    runSQL $ E.select $
             E.from $ \(taskUser `E.InnerJoin` turn) -> do
             E.on (  taskUser E.^. SqlT.TaskUserTaskId
                   E.==. turn E.^. SqlT.TurnTaskId)
             E.orderBy [E.desc (turn E.^. SqlT.TurnStartDate)]
             return (turn E.^. SqlT.TurnUserId)
  let taskUsers :: [Key SqlT.User] = map E.unValue taskUsersValue  -- TODO incorporate in line above with fmapping
  case nextUser unfinishedTurns taskUsers of
    Just nextUser' -> do
        _ <- runSQL $ insert SqlT.Turn {
             SqlT.turnUserId     = nextUser'
           , SqlT.turnTaskId     = taskId
           , SqlT.turnStartDate  = startDate currentTime finishedTurns
           , SqlT.turnFinishedAt = Nothing
           }
        return undefined
    Nothing -> return undefined   -- WARNING should never be evaluated!!!
  where
    startDate currentTime [] = addUTCTime (1800::NominalDiffTime) currentTime
    startDate _currTime (Entity _key lastTurn : _xs) =
      addUTCTime (realToFrac $ SqlT.taskFrequency task)
                 (fromJust $ SqlT.turnFinishedAt lastTurn)
    nextUser [] taskUsers =
      -- head should never be able to fail. If taskUsers is empty `users \\ taskUsers`
      -- will not be empty and therefore skip `head TaskUsers`
      Just $ fromMaybe (head taskUsers) $ listToMaybe $ users \\ taskUsers
    nextUser _unfinishedTurns _ = Nothing

returnNewTask :: SqlT.TaskId -> ApiAction ctx a
returnNewTask taskId = do
  newTask <- trySqlSelectFirst' SqlT.TaskId taskId
  setStatus created201
  let location = sformat ("/tasks/" % int) (integerKey taskId :: Integer)
  setHeader "Location" location
  getTaskInfo json newTask
