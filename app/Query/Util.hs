{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Query.Util
    ( runSQL
    , integerKey
    , trySqlGet
    , trySqlGet'
    , trySqlSelectFirst
    , trySqlSelectFirst'
    , trySqlSelectFirstError
    , sqlAssertIsNotThere
    )
where

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Logger    (runStdoutLoggingT)
import qualified Database.Persist        as P
import           Database.Persist        ((==.))
import           Database.Persist.Sqlite ( SqlBackend
                                         , Key
                                         , ToBackendKey
                                         , runSqlConn
                                         , fromSqlKey
                                         )
import           Web.Spock

import           Model.CoreTypes         (SqlQuery)
import           Util                    (JsonError(..), errorJson)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend)
       => SqlQuery a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

integerKey :: (Num n, ToBackendKey SqlBackend record) => Key record -> n
integerKey = fromIntegral . fromSqlKey

-- TODO combine get and selectFirst or think about why each is necessary
-- TODO figure out a better type signature for all trySql methods
trySqlGet :: (P.PersistEntityBackend b ~ SqlBackend
             , SpockConn (ActionCtxT ctx m) ~ SqlBackend, MonadIO m
             , P.PersistEntity b, HasSpock (ActionCtxT ctx m)
             ) => Key b -> ActionCtxT ctx m b
trySqlGet entityId = do
  mEntity <- runSQL $ P.get entityId
  case mEntity of
    Nothing -> errorJson NotFound
    Just entity -> return entity

-- strict version which crashes if it's not there
trySqlGet' :: ( P.PersistEntityBackend b ~ SqlBackend, SpockConn m ~ SqlBackend
              , P.PersistEntity b, HasSpock m, Monad m
              ) => Key b -> m b
trySqlGet' entityId = do
  mEntity <- runSQL $ P.get entityId
  case mEntity of
    Nothing -> error "I fucked up, this really value really should be there!"
    Just entity -> return entity

trySqlSelectFirst :: ( P.PersistEntityBackend record ~ SqlBackend
                     , SpockConn (ActionCtxT ctx m) ~ SqlBackend
                     , P.PersistField typ
                     , MonadIO m
                     , P.PersistEntity record
                     , HasSpock (ActionCtxT ctx m)
                     ) => P.EntityField record typ
                       -> typ
                       -> ActionCtxT ctx m (P.Entity record)
trySqlSelectFirst = trySqlSelectFirstError NotFound

-- strict version which crashes if it's not there
trySqlSelectFirst' :: ( P.PersistEntityBackend record ~ SqlBackend
                      , SpockConn m ~ SqlBackend , P.PersistField typ
                      , P.PersistEntity record, HasSpock m, Monad m
                      ) => P.EntityField record typ -> typ -> m (P.Entity record)
trySqlSelectFirst' identifier entityId = do
  mEntity <- runSQL $ P.selectFirst [identifier ==. entityId] []
  case mEntity of
    Nothing -> error "I fucked up, this really value really should be there!"
    Just entity -> return entity

trySqlSelectFirstError :: ( P.PersistEntityBackend record ~ SqlBackend
                          , SpockConn (ActionCtxT ctx m) ~ SqlBackend
                          , P.PersistField typ
                          , MonadIO m
                          , P.PersistEntity record
                          , HasSpock (ActionCtxT ctx m)
                          ) => JsonError
                            -> P.EntityField record typ
                            -> typ
                            -> ActionCtxT ctx m (P.Entity record)
trySqlSelectFirstError errStatus identifier entityId = do
  mEntity <- runSQL $ P.selectFirst [identifier ==. entityId] []
  case mEntity of
    Nothing -> errorJson errStatus
    Just entity -> return entity

sqlAssertIsNotThere :: ( P.PersistEntityBackend record ~ SqlBackend
                       , SpockConn (ActionCtxT ctx m) ~ SqlBackend
                       , P.PersistField typ
                       , MonadIO m
                       , P.PersistEntity record
                       , HasSpock (ActionCtxT ctx m)
                       ) => JsonError
                         -> P.EntityField record typ
                         -> typ
                         -> ActionCtxT ctx m ()
sqlAssertIsNotThere errStatus identifier entityId = do
  mEntity <- runSQL $ P.selectFirst [identifier ==. entityId] []
  case mEntity of
    Nothing -> return ()
    Just _entity -> errorJson errStatus
