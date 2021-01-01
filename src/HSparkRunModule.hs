{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-
--https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783286331/1/ch01lvl1sec13/examining-a-json-file-with-the-aeson-package
--http://www.serpentine.com/wreq/tutorial.html
-}

module HSparkRunModule (
  runJobs
  , readStandaloneConfFromFileT
  ) where

import Control.Lens ((&), (^.), (^?), (.~))
import Data.Aeson (FromJSON,ToJSON)
import Data.Aeson.Types
import Data.Aeson.Lens (key)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Control.Exception as E

import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Typeable
import Control.Monad

import Data.Aeson

import SqlDb (SpResponse(..), allQueuedEx, localConnStringIO, SparkCommand, updateSparkAppId, retrievePool)
import StandaloneRun (StandaloneConf(..), Wrapper(..), StandaloneParam, hardcodedConf, generateStandaloneParam, post_StandaloneSubmitE)
import Schema
--import Data.Pool

import qualified Data.Text as TT
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.List
import Control.Exception
import Control.Monad.IO.Class (MonadIO,liftIO)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time

-- FIXME replace env String with Types (DEV | TEST | PROD etc)
runJobs :: String -> IO ()
runJobs env = do
  connectionString <- localConnStringIO env
  pool <- retrievePool connectionString 20
  jobs <- runExceptT (allQueuedEx pool)
  processJobsT env jobs

-- Alias for uuid String ?
submitJobEx :: String -> String -> StandaloneParam -> IO ()
submitJobEx env uuid sp = do
--  print sp
  spResp <- runExceptT (post_StandaloneSubmitE sp)
  processSpResponse env uuid spResp

processSpResponse :: String -> String -> Either String SpResponse -> IO ()
processSpResponse env uuid (Right spResp) = do
  connectionString <- localConnStringIO env
  pool <- retrievePool connectionString 10
  updateSparkAppId pool uuid (submissionId spResp)
processSpResponse env uuid (Left e) = putStrLn ("PROCESS SUBMIT failed with error: " ++ (show e))

readStandaloneConfFromFileT :: MaybeT IO StandaloneConf
readStandaloneConfFromFileT = MaybeT $ do
    input <- C.readFile "standaloneConf.json"
    let mm = decode input :: Maybe Wrapper
    return (fmap (standaloneConf) mm)

processConf :: Maybe StandaloneConf -> IO (StandaloneParam)
processConf (Just conf) = do
  liftIO $ putStrLn ("Loaded StandaloneConf Successfully from the file standaloneConf.json")
  return (standaloneParam conf)
processConf Nothing = do
  liftIO $ putStrLn ("FAILURE LOADING Conf from the file standaloneConf.json  ")
  return hardcodedConf

processJobsT :: String -> Either String [Job] -> IO ()
processJobsT env (Right jobs) = do
  maybeStandaloneConf <- runMaybeT readStandaloneConfFromFileT
  defaultStandaloneParams <- processConf maybeStandaloneConf
  let n = length jobs
  mapConcurrently (execJob env defaultStandaloneParams) jobs
  print (show n ++ " jobs processed")

processJobsT env (Left e) = putStrLn ("PROCESS JOBS failed with error: " ++ (show e))

getUuidAndUpdatedStandaloneParam :: Job -> StandaloneParam -> (String, StandaloneParam)
getUuidAndUpdatedStandaloneParam job defaultStandaloneParams = (uuid,sp) where
  uuid = jobUuid job
  command = jobCommand job
  sparkCommand = decode (LBC.pack command) :: Maybe SparkCommand
  maybeStandaloneParam = (generateStandaloneParam defaultStandaloneParams) <$> sparkCommand
  sp = fromMaybe hardcodedConf maybeStandaloneParam

execJob :: String -> StandaloneParam -> Job -> IO ()
execJob env defaultStandaloneParams job = do
  let (uuid,sp) = getUuidAndUpdatedStandaloneParam job defaultStandaloneParams
  subId <- submitJobEx env uuid sp
  print ("GOT subId" ++ (show subId))
