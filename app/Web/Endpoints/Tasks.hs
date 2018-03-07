{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Endpoints.Tasks where

import           Control.Monad             ((<=<), when, void)
import           Control.Monad.IO.Class
import           Data.HVect                (HVect, ListContains)
import           Data.Maybe                (fromJust, listToMaybe, fromMaybe)
import           Data.List                 ((\\))
import           Data.Time.Clock
import qualified Database.Esqueleto        as E
import           Database.Persist          hiding (delete, get)
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
import           Util                      (runSQL)
import qualified Util
import           Web.Auth                  (authHook)

routeTasks :: Api (HVect xs)
routeTasks =
  prehook authHook $ do
    get "tasks" getTasksAction
    get ("tasks" <//> var) $
      getTaskAction <=< Util.trySqlSelectFirst SqlT.TaskId
    delete ("tasks" <//> var) $ \taskId ->
      Util.trySqlGet taskId >> deleteTaskAction taskId
    post ("tasks" <//> var <//> "finish") $ \taskId ->
      Util.trySqlSelectFirst SqlT.TurnTaskId taskId >> finishTaskAction taskId
    post ("tasks" <//> "update") updateTurnsAction
    put ("tasks" <//> var) $ \taskId ->
      Util.eitherJsonBody >>= putTaskAction taskId
    post "tasks" $ Util.eitherJsonBody >>= postTasksAction

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
  Util.emptyResponse

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
  Util.emptyResponse

putTaskAction :: ListContains n Email xs
              => SqlT.TaskId -> JsonTaskIn.Task -> ApiAction (HVect xs) a
-- TODO combine with postTaskAction
-- TODO maybe to JsonTaskIn.Task with argument pattern matching as well
putTaskAction taskId task = do
  _task <- Util.trySqlGet taskId  -- we just want to check if it is there TODO maybe think of something better
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
  Util.emptyResponse

updateTurns :: ApiAction ctx ()
updateTurns = do
  users <- runSQL $ P.selectList [ SqlT.UserDisabled ==. False ] []
  let userIds = map (\(Entity key _val) -> key) users
  tasks :: [Entity SqlT.Task] <- runSQL $ P.selectList [] []
  mapM_ (updateTaskTurns userIds) tasks

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
  when (null unfinishedTurns) $
    void . runSQL $ insert SqlT.Turn {
         SqlT.turnUserId     = nextUser taskUsers
       , SqlT.turnTaskId     = taskId
       , SqlT.turnStartDate  = startDate currentTime finishedTurns
       , SqlT.turnFinishedAt = Nothing
       }
  Util.emptyResponse
  where
  -- head should never be able to fail. If taskUsers is empty `users \\ taskUsers`
  -- will not be empty and therefore skip `head TaskUsers`
  nextUser taskUsers = fromMaybe (head taskUsers) $ listToMaybe $ users \\ taskUsers
  frequency = SqlT.taskFrequency task
  startDate currentTime [] = addUTCTime (1800::NominalDiffTime) currentTime
  startDate _currTime (Entity _key lastTurn : _xs) =
    addUTCTime (realToFrac frequency)
               (fromJust . SqlT.turnFinishedAt $ lastTurn)

returnNewTask :: SqlT.TaskId -> ApiAction ctx a
returnNewTask taskId = do
  newTask <- Util.trySqlSelectFirst' SqlT.TaskId taskId
  setStatus created201
  let location = sformat ("/tasks/" % int) (Util.integerKey taskId :: Integer)
  setHeader "Location" location
  getTaskInfo json newTask
