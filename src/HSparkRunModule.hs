{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-
--https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783286331/1/ch01lvl1sec13/examining-a-json-file-with-the-aeson-package
--http://www.serpentine.com/wreq/tutorial.html
-}

module HSparkRunModule (
  RunData(..)
  , runJobsReader
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

import SqlDb (SpResponse(..), SparkCommand, SqlParam(..))
import StandaloneRun (StandaloneConf(..), Wrapper(..), StandaloneParam, hardcodedConf, generateStandaloneParam, post_StandaloneSubmitE)
import Schema
--import Data.Pool

import qualified Data.Text as TT
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.List
import Control.Exception
import Control.Monad.IO.Class (MonadIO,liftIO)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time

import JobSource

-- TODO: Replace clunky param passing with ReaderT
-- FIXME replace env String with Types (DEV | TEST | PROD etc)
-- TODO: Alias for uuid String ?
submitJobEx :: String -> StandaloneParam -> SqlParam -> IO ()
submitJobEx uuid sparam sqlParam = do
  spResp <- runExceptT (post_StandaloneSubmitE sparam)
  processSpResponse uuid sqlParam spResp
  return ()

processSpResponse :: String -> SqlParam -> Either String SpResponse -> IO ()
processSpResponse uuid sqlParam (Right spResp) = do
  updateSparkAppId sqlParam uuid (submissionId spResp)
processSpResponse env uuid (Left e) = putStrLn ("PROCESS SUBMIT failed with error: " ++ (show e))

getUuidAndUpdatedStandaloneParam :: Job -> StandaloneParam -> (String, StandaloneParam)
getUuidAndUpdatedStandaloneParam job defaultStandaloneParams = (uuid,sp) where
  uuid = jobUuid job
  command = jobCommand job
  sparkCommand = decode (LBC.pack command) :: Maybe SparkCommand
  maybeStandaloneParam = (generateStandaloneParam defaultStandaloneParams) <$> sparkCommand
  sp = fromMaybe hardcodedConf maybeStandaloneParam

--execJob :: Job -> ReaderT RunData IO ()
execJob ::StandaloneParam -> SqlParam -> Job -> IO ()
execJob sparam sqlParam job = do
--  environment <- ask
--  let (e,sparam) = (env environment, param environment)
  let (uuid,sp) = getUuidAndUpdatedStandaloneParam job sparam
  subId <- submitJobEx uuid sp sqlParam
  liftIO $ print ("GOT subId" ++ (show subId))

runJobsReader :: ReaderT RunData IO ()
runJobsReader = do
  environment <- ask
  let (e,sparam,sqlParam) = (env environment, param environment, sourceParam environment)
  liftIO $ do
    jobs <- runExceptT (allQueued sqlParam)
    js <- processJobs jobs
    let n = length js
    mapConcurrently (execJob sparam sqlParam) js
    print (show n ++  " jobs processed")

getP :: String -> IO SqlParam
getP env = getParam env

data RunData = RunData { env :: String, param :: StandaloneParam, sourceParam :: SqlParam} deriving (Show)

processJobs :: Either String [Job] -> IO [Job]
processJobs (Right js) = return js
processJobs (Left e) = do
  putStrLn ("PROCESS JOBS failed with error: " ++ (show e))
  return []
