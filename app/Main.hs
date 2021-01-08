-- | Main entry point for HSparkRun application

{--
-- | Read args from command line
-- |   HSPARKRUN_ENV=dev HSPARKRUN_HOME=$(pwd) HSPARKRUN_TYPE=standalone
-- |
-- |     HSPARKRUN_TYPE : possible values (yarn/standalone) depending upon cluster manager for spark
-- |    HSPARKRUN_ENV = dev / test / prod
-- |    HSPARKRUN_HOME = $(pwd)    # current directory for spark run app
-- |
-- |   Workflow:
-- |    * Read above mentioned arguments from command line and set the mode of execution
-- |    * load spark run conf ( yarn or standalone configuration properties to reach cluster manager )
-- |    * initialize source param (sql/file/... others)
-- |    * Prepare RunData module
-- |    * Run the module HSparkRunModule via the reader
--}
module Main where

import StandaloneRun (StandaloneParam,readConf,evalConf)
import Schema (Job)
import SqlDb (SqlParam)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import HSparkRunModule (RunData(..),runJobsReader)
import JobSource

import System.Environment   
import Data.List
import Data.Typeable

{-|
 Read args from command line

     HSPARKRUN_TYPE : possible values (yarn/standalone) depending upon cluster manager for spark

     HSPARKRUN_ENV = dev / test / prod

     HSPARKRUN_HOME = $(pwd)    # current directory for spark run app

   Workflow:
   
    * Read above mentioned arguments from command line and set the mode of execution
    
    * load spark run conf ( yarn or standalone configuration properties to reach cluster manager )
    
    * initialize source param (sql/file/... others)
    
    * Prepare RunData module
    
    * Run the module HSparkRunModule via the reader
-}

-- FIXME: pick command line env with default values
main :: IO ()
main = do
--  args <- getArgs                  -- IO [String]
  maybeConf <- runMaybeT readConf
  param <- evalConf maybeConf
  sqlParam <- getParam "dev"
  let runData = RunData{env = "dev", param = param, sourceParam = sqlParam}
  op <- runReaderT runJobsReader runData
  return ()
