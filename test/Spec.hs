{-
  https://begriffs.com/posts/2016-05-14-pragmatic-haskell-1.html
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT(..))
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Database.Persist ((>=.), deleteWhere)
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Postgresql (runMigration, runSqlConn, withPostgresqlConn, ConnectionString)
import Test.Hspec (Spec, SpecWith, shouldBe, describe, hspec, it, before, after)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, runNoLoggingT, LoggingT)

import qualified Data.Text as T
import Schema (Job(..))
import Schema
import SqlDb (migrateDb, createJob, fetchJob, deleteJob, fetchJobStatus)

import TestUtils (setupTests)

import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Maybe (isJust,fromMaybe)

import SqlDb (SparkCommand(..),SpResponse(..))
import HSparkRunModule (runJobs)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time
import Control.Exception

hardCodedTestConnString :: ConnectionString
hardCodedTestConnString = "host=127.0.0.1 port=5432 user=sauser dbname=hspark_test"

main :: IO ()
main = do
  (dbConn) <- setupTests

  hspec $ before (beforeHook1 dbConn) spec1
  hspec $ before (beforeHook2 dbConn) $ after (afterHook dbConn) $ spec2
  hspec $ before (beforeHook3 dbConn) $ after (afterHook3 dbConn) $ spec3
  return ()

beforeHook1 :: ConnectionString -> IO (Bool)
beforeHook1 dbConn = do
  inDb <- isJust <$> fetchJob dbConn 1
  return (inDb)

spec1 :: SpecWith (Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "There should be no job in Postgres" $ \(inDb) -> inDb `shouldBe` False

beforeHook2 :: ConnectionString -> IO (Bool, Int64)
beforeHook2 dbConn = do
  c <- getCurrentTime
  testJob <- dummyJob ("1_ ASDFSDF")
  jobKeyEither <-  try(createJob dbConn testJob) :: IO (Either SomeException Int64)
  case jobKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right jobKey -> do
      inDb <- isJust <$> fetchJob dbConn jobKey
      return (inDb, jobKey)

spec2 :: SpecWith (Bool, Int64)
spec2 = describe "After creating the job but not fetching" $ do
  it "There should be a job in Postgres" $ \(inDb, _) -> inDb `shouldBe` True

beforeHook3 :: ConnectionString -> IO (Bool, Bool, Int64)
beforeHook3 dbConn  = do
  testJob <- dummyJob ("2_ ASDFSDF")
  jobKeyEither <-  try(createJob dbConn testJob) :: IO (Either SomeException Int64)
  case jobKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right jobKey -> do
      inDb <- isJust <$> fetchJob dbConn jobKey
      _ <- runJobs "test"
      status <- fetchJobStatus dbConn jobKey
      let isSubmitted = fmap (\x -> x == "Submitted")  status
      let isSub = fromMaybe False isSubmitted
      return (inDb, isSub, jobKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the job and submitting the job" $ do
  it "There should be a job in Postgres" $ \(inDb, _, _) -> inDb `shouldBe` True
  it "The job status should be Submitted" $ \(_, isSub, _) -> isSub `shouldBe` True

dummyJob :: String -> IO (Job)
dummyJob dummyId =  do
  c <- getCurrentTime
  let scmd = SparkCommand {
    sparkClass = "org.apache.spark.examples.SparkPi"
    , applicationJar = "/opt/spark-2.4.0-bin-hadoop2.7/examples/jars/spark-examples_2.11-2.4.0.jar"
    , argList = ["80"]
  }
  let sc = unpack ( encode ( scmd ) )
  let j = Job {
    jobUuid = dummyId
   , jobCommand = sc
   , jobSparkJobId = Nothing
   , jobSparkJobAppName = Nothing
   , jobStartJobTime = Nothing
   , jobEndJobTime = Nothing
   , jobErrorMsg = Nothing
   , jobStatus = (Just "Queued")
   , jobCreatedAt = c
   , jobUpdatedAt = c
   , jobExecutedByUser = Nothing
   , jobCtxId = Nothing

  }
  return (j)

afterHook :: ConnectionString -> (Bool, Int64) -> IO ()
afterHook dbConn (_, key) = do
  deleteJob dbConn key
  return ()

afterHook3 :: ConnectionString -> (Bool, Bool,Int64) -> IO ()
afterHook3 dbConn (_, _, key) = do
  deleteJob dbConn key
  return ()

