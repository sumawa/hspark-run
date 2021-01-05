{-# LANGUAGE OverloadedStrings #-}
module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad.Logger (runStdoutLoggingT,runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent, runMigration, ConnectionString, runSqlPersistMPool, withPostgresqlPool)
import SqlDb (SqlParam(..))
import Schema (migrateAll,Job)
import HSparkRunModule (runJobsReader,RunData(..))
import StandaloneRun (readConf,evalConf)
import Control.Monad.Trans.Maybe
import JobSource

runActionWithPoolTest connectionString poolSize action = runStdoutLoggingT $ withPostgresqlPool connectionString poolSize $ \pool ->
  liftIO $ do
    runSqlPersistMPool action pool

setupTests = do
  maybeConf <- runMaybeT readConf
  param <- evalConf maybeConf
  sqlParam <- getParam "test"
  let runData = RunData{env = "test", param = param, sourceParam = sqlParam}
  migrateDb sqlParam
  return runData

--setupTests :: IO (ConnectionString)
--setupTests = do
--  dbInfo <- localConnStringIO "test"
--  runStdoutLoggingT $ withPostgresqlConn dbInfo $ \dbConn ->
--    runReaderT (runMigrationSilent migrateAll) dbConn
----  tid <- forkIO runServer
----  threadDelay 1000000
--  return (dbInfo)
