{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.CoreTypes where

import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Web.Spock

type Api ctx = SpockCtxM ctx SqlBackend () () ()
type ApiAction ctx a = SpockActionCtx ctx SqlBackend () () a


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  email     Text
  password  Text
  salt      Text
  firstName Text
  lastName  Text

  deriving Show

Token
  userId     UserId
  tokenId    Text
  validUntil UTCTime

TaskUser
  taskId TaskId
  userId UserId
  UniqueTaskUser taskId userId

Turn json
  userId UserId
  date   UTCTime

Task json
  title          Text
  frequency      Int
  completionTime Int
  nextTurn       TurnId
|]
