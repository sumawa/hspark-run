module Main where

--import Lib
import StandaloneRun (StandaloneParam,readConf,evalConf)
import Schema (Job)
import HSparkRunModule (Environment(..),runJobsReader,readStandaloneConfFromFileT)
import SqlDb (localConnStringIO, retrievePool,allQueuedEx)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

main :: IO ()
--main = undefined
main = do
  connectionString <- localConnStringIO "dev"
  pool <- retrievePool connectionString 20
  queuedJobs <- runExceptT (allQueuedEx pool)
  maybeStandaloneConf <- runMaybeT readConf
  param <- evalConf maybeStandaloneConf
  let env = Environment{jobs = processJobs queuedJobs, param = param}
  op <- runReaderT runJobsReader env
  return ()

processJobs :: Either String [Job] -> [Job]
processJobs (Right js) = js 
processJobs (Left e) = []



