{-# LANGUAGE OverloadedStrings #-}
module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad.Logger (runStdoutLoggingT,runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent, runMigration, ConnectionString, runSqlPersistMPool, withPostgresqlPool)
import SqlDb (createJob, deleteJob, localConnStringIO)
import Schema (migrateAll,Job)
import HSparkRunModule (runJobsReader,RunData)
import StandaloneRun (readConf,evalConf)
import Control.Monad.Trans.Maybe

runActionWithPoolTest connectionString poolSize action = runStdoutLoggingT $ withPostgresqlPool connectionString poolSize $ \pool ->
  liftIO $ do
    runSqlPersistMPool action pool

runActionWithPoolTest1 connectionString poolSize = runStdoutLoggingT $ withPostgresqlPool connectionString poolSize $ \pool -> do return pool

runNoLoggingActionWPoolTest connectionString action = runNoLoggingT $ withPostgresqlPool connectionString 10 $ \pool ->
  liftIO $ do
    runSqlPersistMPool action pool

setupTests = do
  dbConn <- localConnStringIO "test"
  pool <- runActionWithPoolTest1 dbConn (10)
  print pool
--  maybeConf <- runMaybeT readConf
--  param <- evalConf maybeConf
--  let runData = RunData{env = "dev", jobs = processJobs queuedJobs, param = param}
--  runSqlPersistMPool dbConn 10 (runMigration migrateAll)
  runActionWithPoolTest dbConn 10 (runMigration migrateAll)
  return pool

--processJobs :: Either String [Job] -> [Job]
--processJobs (Right js) = js
--processJobs (Left e) = []

--  dbConn
--  runStdoutLoggingT $ withPostgresqlConn dbInfo $ \dbConn ->
--    runReaderT (runMigrationSilent migrateAll) dbConn
----  tid <- forkIO runServer
----  threadDelay 1000000
--  return (dbInfo)

--setupTests :: IO (ConnectionString)
--setupTests = do
--  dbInfo <- localConnStringIO "test"
--  runStdoutLoggingT $ withPostgresqlConn dbInfo $ \dbConn ->
--    runReaderT (runMigrationSilent migrateAll) dbConn
----  tid <- forkIO runServer
----  threadDelay 1000000
--  return (dbInfo)
