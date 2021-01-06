module TestSqlDb(
  testCreateJob
--  , testDeleteJob
--  , testMultiple
) where

import StandaloneRun (evalConf,readConf)
import SqlDb (SqlParam)
import SparkRun
import JobSource
import HSparkRunModule (runJobsReader,RunData(..))
import Schema

import Data.ByteString.Lazy.Char8 as Char8
import Data.Aeson
import Data.Time

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

testCreateJob :: String -> IO ()
testCreateJob env =  do
  maybeConf <- runMaybeT readConf
  param <- evalConf maybeConf
  sqlParam <- getParam "dev"
  let runData = RunData{env = "dev", param = param, sourceParam = sqlParam}
  c <- getCurrentTime
  let scmd = SparkCommand {
    sparkClass = "org.apache.spark.examples.SparkPi"
    , applicationJar = "/opt/spark-2.4.0-bin-hadoop2.7/examples/jars/spark-examples_2.11-2.4.0.jar"
    , argList = ["80"]
  }
  let sc = Char8.unpack ( encode ( scmd ) )
--  print sc
--  print (typeOf sc)
  let j = Job  ( (show c) ++ " ASDFSDF")  sc Nothing Nothing Nothing Nothing Nothing (Just "Queued") c c Nothing Nothing
  inserted <- createJob sqlParam j
  print ""
--
