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
    -- post new Turn
    currentTime <- liftIO getCurrentTime
    allUsers <- runSQL $ selectKeysList [] [Asc SqlT.UserId]
    turnId <- runSQL $ insert (SqlT.Turn (Prelude.head allUsers) currentTime)

    taskId <- runSQL $ insert SqlT.Task {
                                          SqlT.taskTitle = "title"
                                        , SqlT.taskFrequency = 1
                                        , SqlT.taskCompletionTime = 1
                                        , SqlT.taskNextTurn = turnId
                                        }

    allTasks <- runSQL $ selectList [] [Asc SqlT.TaskId]
    json $ map JsonTask.jsonTask allTasks
  post "tasks" $ do
    --maybeTask <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonTask.Task)
    maybeTask <- jsonBody :: SqlT.ApiAction ctx (Maybe JsonTurn.Turn)
    case maybeTask of
      Nothing -> do
        setStatus badRequest400
        errorJson Util.BadRequest
      Just task -> do
        setStatus created201
        currentTime <- liftIO getCurrentTime
        --allUsers <- runSQL $ selectKeys [] [Asc SqlT.UserId]
        let allUsers = ["foo"]
        --newId <- createTask task currentTime
        --json $ object ["result" .= String "success", "id" .= newId]
        text . T.pack . show $ allUsers

--createTask turn time = runSQL $ insert turn
--      where turn = SqlT.Turn (1::Int) time
--createTask task = runSQL $ insert task
--    where task = SqlT.Task
--                    { SqlT.title          = JsonRegistration.title task
--                    , SqlT.frequency      = JsonRegistration.frequency task
--                    , SqlT.completionTime = JsonRegistration.completionTime task
--                    , SqlT.users          = JsonRegistration.users task
--                    , SqlT.nextTurn       = firstTurn
--                    }
--          firstTurn = ;
