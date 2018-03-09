{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.SqlTypes where

import           Data.Maybe              (isNothing)
import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime)
import           Database.Persist.TH     ( persistLowerCase
                                         , mkMigrate
                                         , mkPersist
                                         , sqlSettings
                                         , share
                                         )

import           Model.CoreTypes         (Email)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  email      Email
  password   Text
  salt       Text
  firstName  Text
  lastName   Text
  verifyCode Text Maybe
  disabled   Bool
  absent     Bool

  UniqueUserEmail email

  deriving Show

Token
  userId     UserId
  tokenId    Text
  validUntil UTCTime

  UniqueTokenId

TaskUser
  taskId TaskId
  userId UserId

  UniqueTaskUser taskId userId

Turn json
  userId     UserId
  taskId     TaskId
  startDate  UTCTime
  finishedAt UTCTime Maybe

Task json
  title          Text
  description    Text
  frequency      Int
  completionTime Int

Invitation json
  email Email
  code  Text
  deriving Show

  UniqueInvitationEmail email
|]

userIsVerified :: User -> Bool
userIsVerified = isNothing . userVerifyCode
