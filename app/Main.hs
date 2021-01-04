module Main where

--import Lib
import StandaloneRun (StandaloneParam,readConf,evalConf)
import Schema (Job)
import HSparkRunModule (RunData(..),runJobsReader)
import SqlDb (localConnStringIO, retrievePool,allQueuedEx)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  connectionString <- localConnStringIO "dev"
  pool <- retrievePool connectionString 20
  queuedJobs <- runExceptT (allQueuedEx pool)
  maybeConf <- runMaybeT readConf
  param <- evalConf maybeConf
  let environment = RunData{env = "dev", jobs = processJobs queuedJobs, param = param}
  print $ length (jobs environment)
  op <- runReaderT runJobsReader environment
  return ()

processJobs :: Either String [Job] -> [Job]
processJobs (Right js) = js
processJobs (Left e) = []



