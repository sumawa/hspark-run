{-
  References for Persitent
  https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md
  https://begriffs.com/posts/2016-06-01-pragmatic-haskell-2.html
  https://www.parsonsmatt.org/2019/12/06/splitting_persistent_models.html
  https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db
  https://github.com/haskell-servant/example-servant-persistent/blob/master/src/Models.hs
  https://www.yesodweb.com/book/persistent

  https://begriffs.com/posts/2016-05-14-pragmatic-haskell-1.html
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module SqlDb (
  createJob
  , deleteJob
  , hardCodedConnString
  , localConnStringIO
  , migrateDb
  , fetchJob
  , SpResponse(..)
  , SparkCommand(..)
  , updateSparkAppId
  , allQueuedEx
  , readDbConfFromFileT
  , processDbConf
  , fetchJobStatus
  , runActionWithPool
) where

import Data.Int (Int64)

import Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT, runNoLoggingT, NoLoggingT, LogLevel(..), filterLogger)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad (void,join)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Database.Persist (selectList, (==.), (<.), (=.), SelectOpt(..), Entity, entityValues, entityVal)
import Database.Persist (get,getEntity,getJustEntity,insert,delete,selectList,selectFirst,update, updateWhere)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist (PersistEntity)

import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT,SqlBackend, withPostgresqlPool, runSqlPersistMPool)

import Data.ByteString.Char8 (pack,unpack)
import qualified Data.ByteString.Char8 as C

import Data.ByteString.Lazy.Char8 as Char8

import qualified Data.Text as T
import Data.Aeson (FromJSON,ToJSON)
import Data.Aeson

import Schema (Job)
import Schema
import Data.Time
import GHC.Generics (Generic)
import Data.Text
import Data.Typeable
import Control.Exception

data SpResponse = SpResponse { action :: Text
  , driverState :: Maybe Text, message :: Maybe Text, serverSparkVersion :: Text
  , submissionId :: String, success :: Bool } deriving (Show,Generic)

instance FromJSON SpResponse
instance ToJSON SpResponse

data SparkCommand = SparkCommand { sparkClass :: String
  , applicationJar :: String
  , argList :: [String]
} deriving (Show,Generic)

instance FromJSON SparkCommand
instance ToJSON SparkCommand


logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = True
logFilter _ (LevelOther _) = False

hardCodedConnString :: ConnectionString
hardCodedConnString = "host=127.0.0.1 port=5432 user=sauser dbname=hspark_dev"

localConnStringIO :: String -> IO ConnectionString
localConnStringIO env = do
  dbConf <- runMaybeT $ readDbConfFromFileT env
  processDbConf dbConf

--runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
--runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
--  runReaderT action backend

runActionWithPool connectionString action = runStdoutLoggingT $ withPostgresqlPool connectionString 10 $ \pool ->
  liftIO $ do
    runSqlPersistMPool action pool

--runNoLoggingAction :: ConnectionString -> SqlPersistT (NoLoggingT IO) a ->  IO a
--runNoLoggingAction connectionString action = runNoLoggingT $ withPostgresqlConn connectionString $ \backend ->
--  runReaderT action backend

runNoLoggingActionWPool connectionString action = runNoLoggingT $ withPostgresqlPool connectionString 10 $ \pool ->
  liftIO $ do
    runSqlPersistMPool action pool

migrateDb :: ConnectionString -> IO ()
migrateDb connString = runActionWithPool connString (runMigration migrateAll)

initSqlDbMain :: String -> IO ()
initSqlDbMain env = do
  connectionString <- localConnStringIO env
  migrateDb connectionString

createJob :: ConnectionString -> Job -> IO Int64
createJob connString job = fromSqlKey <$> runActionWithPool connString (insert job)

deleteJob :: ConnectionString -> Int64 -> IO ()
deleteJob connString uuid = runActionWithPool connString (delete jobKey) where
  jobKey :: Key Job
  jobKey = toSqlKey uuid

fetchJob :: ConnectionString -> Int64 -> IO (Maybe Job)
fetchJob connString jid = runActionWithPool connString (get (toSqlKey jid))

fetchJobStatus :: ConnectionString -> Int64 -> IO (Maybe String)
fetchJobStatus connString jid = do
  job <- fetchJob connString jid
  let status = fmap (jobStatus) job
  return (join status)

jobsAllQueuedEx = do
  jobEntityList <- selectList [JobStatus ==. Just "Queued"][]
  let c = fmap (\x ->  entityVal x) jobEntityList
  return c

allQueuedEx :: ConnectionString -> ExceptT String IO [Job]
allQueuedEx connString = ExceptT $  do
  result <- try(runNoLoggingActionWPool connString (jobsAllQueuedEx)) :: IO (Either SomeException [Job])
  case result of
    Left ex  -> return (Left $ "Caught exception  : " ++ (show ex) )
    Right val -> return (Right val)

updateSparkAppId :: ConnectionString -> String -> String -> IO ()
updateSparkAppId connString uuid sparkAppId = runActionWithPool connString (updateWhere [JobUuid ==. uuid][JobSparkJobId =. (Just sparkAppId), JobStatus =. (Just "Submitted")] )

data DbConf = DbConf{
  host :: String
  , port :: Int
  , name :: String
  , user :: String
  , pass :: String
  , dbType :: String
  , poolSize :: Int
} deriving (Show,Generic)

instance FromJSON DbConf
instance ToJSON DbConf

readDbConfFromFileT :: String -> MaybeT IO DbConf
readDbConfFromFileT env = MaybeT $ do
    input <- Char8.readFile ("dbConf_" ++ env ++ ".json")
    let mm = decode input :: Maybe DbConf
    return mm

processDbConf :: Maybe DbConf -> IO (ConnectionString)
processDbConf (Just conf) = do
  liftIO $ C.putStrLn ("Loaded StandaloneConf Successfully from the file dbConf.json")
  let str = "host=" ++ (host conf) ++ " port=" ++ (show (port conf)) ++ " user=" ++ (user conf) ++ " dbname=" ++ (name conf)
  return (C.pack str)
processConf Nothing = do
  liftIO $ C.putStrLn ("FAILURE LOADING Conf from the file dbConf.json  ")
  return hardCodedConnString

