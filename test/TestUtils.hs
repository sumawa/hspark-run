module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent, ConnectionString)
import SqlDb (createJob, deleteJob, localConnStringIO)
import Schema (migrateAll)

setupTests :: IO (ConnectionString)
setupTests = do
  dbInfo <- localConnStringIO "test"
  runStdoutLoggingT $ withPostgresqlConn dbInfo $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
--  tid <- forkIO runServer
--  threadDelay 1000000
  return (dbInfo)
