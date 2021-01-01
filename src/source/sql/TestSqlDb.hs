module TestSqlDb(
  testCreateJob
  , testDeleteJob
--  , testMultiple
) where

import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT,runSqlPersistMPool)
import SqlDb (SparkCommand(..),SpResponse(..))
import Data.ByteString.Lazy.Char8 as Char8

import Data.Int (Int64)
import Data.Time

import Schema (Job)
import Schema
import SqlDb (createJob, deleteJob, localConnStringIO, retrievePool)
import Database.Persist (get,insert,delete,selectList,update, updateWhere)
import Database.Persist (selectList, (==.), (<.), (=.), SelectOpt(..), Entity, entityValues, entityVal)

import Control.Monad.IO.Class (MonadIO,liftIO)

import Data.Aeson


testCreateJob :: String -> IO ()
testCreateJob env =  do
  conn <- localConnStringIO env
  pool <- retrievePool conn 10
  print pool
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
  inserted <- createJob pool j
  print ""

--testCreateJobWPool :: String -> IO ()
--testCreateJobWPool env =  do
--  conn <- localConnStringIO env
--  print conn
--  c <- getCurrentTime
--  let scmd = SparkCommand {
--    sparkClass = "org.apache.spark.examples.SparkPi"
--    , applicationJar = "/opt/spark-2.4.0-bin-hadoop2.7/examples/jars/spark-examples_2.11-2.4.0.jar"
--    , argList = ["80"]
--  }
--  let sc = Char8.unpack ( encode ( scmd ) )
----  print sc
----  print (typeOf sc)
--  let j = Job  ( (show c) ++ " ASDFSDF")  sc Nothing Nothing Nothing Nothing Nothing (Just "Queued") c c Nothing Nothing
--  inserted <- createJobWPool conn j
--  print ""

testDeleteJob :: String -> Int64 -> IO()
testDeleteJob env id = do
  conn <- localConnStringIO env
  pool <- retrievePool conn 10
  deleteJob pool id

--testMultiple :: String -> IO ()
--testMultiple env = do
--  connectionString <- localConnStringIO env
--  pool <- retrievePool connectionString 10
--  runSqlPersistMPool ( $ do
--    entityJobs <- selectList [JobStatus ==. Just "Queued"][]
--    let c = fmap (\x ->  entityVal x) entityJobs
--    liftIO $ print (Prelude.length c)
--    liftIO $ print c ) pool

