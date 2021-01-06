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

-- FIXME: pick command line env with default values
main :: IO ()
main = do
  args <- getArgs                  -- IO [String]
  progName <- getProgName          -- IO String
  putStrLn "The arguments are:"  
  mapM putStrLn args  
  putStrLn "The program name is:"  
  putStrLn progName
  maybeConf <- runMaybeT readConf
  param <- evalConf maybeConf
  sqlParam <- getParam "dev"
  let runData = RunData{env = "dev", param = param, sourceParam = sqlParam}
  op <- runReaderT runJobsReader runData
  return ()

processJobs :: Either String [Job] -> [Job]
processJobs (Right js) = js
processJobs (Left e) = []





