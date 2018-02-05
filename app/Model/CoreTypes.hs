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
import qualified Web.Spock

type Api ctx = Web.Spock.SpockCtxM ctx SqlBackend () () ()
type ApiAction ctx a = Web.Spock.SpockActionCtx ctx SqlBackend () () a


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  email     Text
  password  Text
  salt      Text
  firstName Text
  lastName  Text
  verified  Bool

  UniqueUserEmail email

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
  taskId TaskId
  date   UTCTime

Task json
  title          Text
  description    Text
  frequency      Int
  completionTime Int

Invitation json
  email Text
  code  Text Maybe
  deriving Show

  UniqueInvitationEmail email
|]
