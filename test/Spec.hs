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
import Database.Persist.Sql (toSqlKey,SqlBackend)
import Database.Persist.Postgresql (runMigration, runSqlConn, withPostgresqlConn, ConnectionString)
import Test.Hspec (Spec, SpecWith, shouldBe, describe, hspec, it, before, after)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, runNoLoggingT, LoggingT)

import qualified Data.Text as T
import Schema (Job(..))
import Schema
import SqlDb (migrateDb, createJob, fetchJob, deleteJob, fetchJobStatus)
import Data.Pool (Pool)

import TestUtils (setupTests)

import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Maybe (isJust,fromMaybe)

import SqlDb (SparkCommand(..),SpResponse(..))
import HSparkRunModule (runJobs)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time
import Control.Exception

main :: IO ()
main = do
  (pool) <- setupTests
  print pool

  hspec $ before (beforeTest1 pool) spec1
  hspec $ before (beforeTest2 pool) $ after (afterTest pool) $ spec2
  hspec $ before (beforeTest3 pool) $ after (afterTest3 pool) $ spec3
  return ()

beforeTest1 :: (Pool SqlBackend) -> IO (Bool)
beforeTest1 pool = do
  inDb <- isJust <$> fetchJob pool 1
  return (inDb)

spec1 :: SpecWith (Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "There should be no job in Postgres" $ \(inDb) -> inDb `shouldBe` False

afterTest :: (Pool SqlBackend) -> (Bool, Int64) -> IO ()
afterTest pool (_, key) = do
  deleteJob pool key
  return ()

afterTest3 :: (Pool SqlBackend) -> (Bool, Bool,Int64) -> IO ()
afterTest3 pool (_, _, key) = do
  deleteJob pool key
  return ()

beforeTest2 :: (Pool SqlBackend) -> IO (Bool, Int64)
beforeTest2 pool = do
  c <- getCurrentTime
  testJob <- dummyJob ("1_ ASDFSDF")
  jobKeyEither <-  try(createJob pool testJob) :: IO (Either SomeException Int64)
  case jobKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right jobKey -> do
      inDb <- isJust <$> fetchJob pool jobKey
      return (inDb, jobKey)

spec2 :: SpecWith (Bool, Int64)
spec2 = describe "After creating the job but not fetching" $ do
  it "There should be a job in Postgres" $ \(inDb, _) -> inDb `shouldBe` True

beforeTest3 :: (Pool SqlBackend) -> IO (Bool, Bool, Int64)
beforeTest3 pool  = do
  testJob <- dummyJob ("2_ ASDFSDF")
  jobKeyEither <-  try(createJob pool testJob) :: IO (Either SomeException Int64)
  case jobKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right jobKey -> do
      inDb <- isJust <$> fetchJob pool jobKey
      _ <- runJobs pool "test"
      status <- fetchJobStatus pool jobKey
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


