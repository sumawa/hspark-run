
{-
  References for Persistent

  https://www.parsonsmatt.org/2019/12/06/splitting_persistent_models.html
  https://begriffs.com/posts/2016-06-01-pragmatic-haskell-2.html
  https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db
  https://www.yesodweb.com/book/persistent

  https://hackage.haskell.org/package/persistent

-}

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject, Object)
import Data.Aeson.Types (Parser, Pair)

import Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH
import Data.Text (Text)
import Data.Time

{-
  CREATE TABLE jobs (
    id serial PRIMARY KEY,
    uuid varchar(36) UNIQUE NOT NULL,
    command text NOT NULL,
    spark_job_id text DEFAULT NULL,
    spark_job_app_name text DEFAULT NULL,
    start_job_time timestamp DEFAULT NULL,
    end_job_time timestamp DEFAULT NULL,
    error text DEFAULT NULL,
    status   text DEFAULT NULL,
    created_at timestamp NOT NULL DEFAULT current_timestamp,
    updated_at timestamp NOT NULL DEFAULT current_timestamp,
    executed_by_user integer  DEFAULT NULL,
    ctx_id integer DEFAULT NULL
  );
-}

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Job sql=jobs
  uuid String
  command String
  sparkJobId String Maybe default=NULL
  sparkJobAppName Text Maybe default=NULL
  startJobTime UTCTime Maybe default=NULL
  endJobTime UTCTime Maybe default=NULL
  errorMsg Text Maybe default=NULL
  status String Maybe default=NULL
  createdAt UTCTime default=CURRENT_TIMESTAMP
  updatedAt UTCTime default=CURRENT_TIMESTAMP
  executedByUser Int Maybe default=NULL
  ctxId Int Maybe default=NULL
  UniqueUuid uuid
  deriving Show Read
  |]

instance ToJSON Job where
  toJSON job = object
    ["uuid" .= jobUuid job
    , "command" .= jobCommand job
    , "sparkJobId" .= jobSparkJobId job
    , "sparkJobAppName" .= jobSparkJobAppName job
    , "startJobTime" .= jobStartJobTime job
    , "endJobTime" .= jobEndJobTime job
    , "errorMsg" .= jobErrorMsg job
    , "status" .= jobStatus job
    , "createdAt" .= jobCreatedAt job
    , "updatedAt" .= jobUpdatedAt job 
    , "executedByUser" .= jobExecutedByUser job
    , "ctxId" .= jobCtxId job   
    ]

instance FromJSON Job where
  parseJSON = withObject "Job" parseJob

parseJob :: Object -> Parser Job
parseJob o = do
  uJobUuid <- o .: "uuid"
  uJobCommand <- o .: "command"
  uJobSparkJobId <- o .: "sparkJobId"
  uJobSparkJobAppName <- o .: "sparkJobAppName"
  uJobStartJobTime <- o .: "startJobTime"
  uJobEndJobTime <- o .: "endJobTime"
  uJobErrorMsg <- o .: "errorMsg"
  uJobStatus <- o .: "status"
  uJobCreatedAt <- o .: "createdAt"
  uJobUpdatedAt <- o .: "updatedAt"
  uJobExecutedByUser <- o .: "executedByUser"
  uJobCtxId <- o .: "ctxId"    
  return Job {
   jobUuid = uJobUuid
   , jobCommand = uJobCommand
   , jobSparkJobId = uJobSparkJobId
   , jobSparkJobAppName = uJobSparkJobAppName
   , jobStartJobTime = uJobStartJobTime
   , jobEndJobTime = uJobEndJobTime
   , jobErrorMsg = uJobErrorMsg
   , jobStatus = uJobStatus
   , jobCreatedAt = uJobCreatedAt
   , jobUpdatedAt = uJobUpdatedAt
   , jobExecutedByUser = uJobExecutedByUser
   , jobCtxId = uJobCtxId
  }

