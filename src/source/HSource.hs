{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module HSource where

import Data.Aeson (FromJSON,ToJSON)
import Data.Aeson.Types
import Data.Aeson.Lens (key)
import GHC.Generics

import SqlDb (allQueuedEx,SqlParam(..),getSqlParam,jobsAllQueuedEx)
import Schema (Job)
import Schema
import Data.Int (Int64)

import Database.Persist (selectList, (==.), (<.), (=.), SelectOpt(..), Entity, entityValues, entityVal)
import Database.Persist (get,getEntity,getJustEntity,insert,delete,selectList,selectFirst,update, updateWhere)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist (PersistEntity)

import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT,SqlBackend, withPostgresqlPool, runSqlPersistMPool)
import Data.Pool (Pool)
import Data.ByteString.Char8 (pack,unpack)
import qualified Data.ByteString.Char8 as C

import Control.Monad.Except
import Control.Monad (join)
import Control.Exception

class HSource v where
  fetchJob :: v -> Int64 -> IO (Maybe Job)
  createJob :: v  -> Job -> IO Int64
  migrateDb :: v  -> IO ()
  deleteJob :: v -> Int64 -> IO ()
  fetchJobStatus :: v -> Int64 -> IO (Maybe String)
  updateSparkAppId :: v  -> String -> String -> IO ()
  getParam ::  String -> IO v
  allQueued :: v -> ExceptT String IO [Job]

instance HSource (SqlParam) where
  fetchJob :: SqlParam -> Int64 -> IO (Maybe Job)
  fetchJob (SqlParam pool) jid = runSqlPersistMPool (get (toSqlKey jid)) pool

  migrateDb :: SqlParam  -> IO ()
  migrateDb (SqlParam pool) = runSqlPersistMPool (runMigration migrateAll) pool

  deleteJob :: SqlParam  -> Int64 -> IO ()
  deleteJob (SqlParam pool) uuid = runSqlPersistMPool (delete jobKey) pool where
    jobKey :: Key Job
    jobKey = toSqlKey uuid

  fetchJobStatus :: SqlParam -> Int64 -> IO (Maybe String)
  fetchJobStatus sqlParam jid = do
    job <- fetchJob sqlParam jid
    let status = fmap (jobStatus) job
    return (join status)

  updateSparkAppId :: SqlParam  -> String -> String -> IO ()
  updateSparkAppId (SqlParam pool) uuid sparkAppId = runSqlPersistMPool (updateWhere [JobUuid ==. uuid][JobSparkJobId =. (Just sparkAppId), JobStatus =. (Just "Submitted")] ) pool

  getParam :: String -> IO SqlParam
  getParam env = getSqlParam env 10

  createJob :: SqlParam  -> Job -> IO Int64
  createJob (SqlParam pool) job = fromSqlKey <$> runSqlPersistMPool (insert job) pool
  
  allQueued :: SqlParam -> ExceptT String IO [Job]
  allQueued (SqlParam pool) = ExceptT $  do
                                result <- try(runSqlPersistMPool (jobsAllQueuedEx) pool) :: IO (Either SomeException [Job])
                                case result of
                                  Left ex  -> return (Left $ "Caught exception  : " ++ (show ex) )
                                  Right val -> return (Right val)

