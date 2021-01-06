{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-
  References
  http://www.serpentine.com/wreq/tutorial.html
  --https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783286331/1/ch01lvl1sec13/examining-a-json-file-with-the-aeson-package
-}
module StandaloneRun (
  StandaloneParam(..)
  , StandaloneConf(..)
  , Wrapper(..)
  , hardcodedConf
  , readConf
  , evalConf
  ) where

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
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Network.Wreq as W
import Schema (Job)
import Schema
import Data.Maybe
import Data.Map

import Control.Monad.Trans.Maybe
import Control.Exception

--data SparkRunException
--  = SubmitFailed
--  | FetchFailed
--  deriving (Show,Typeable)
--
--instance Exception SparkRunException

-- Could be used for generic request / response processing
data GetBody = GetBody {
    headers :: Map Text Text
  , args :: Map Text Text
  , origin :: Text
  , url :: Text
  } deriving (Show, Generic)

-- Get GHC to derive a FromJSON instance for us.

instance FromJSON GetBody
instance ToJSON GetBody

data EnvVar = EnvVar {
    sp :: Text
    , spark_user :: Text
} deriving (Show,Generic)

data StandaloneParam = StandaloneParam{
    action :: String
    , appResource :: String
    , sparkProperties :: Maybe (Map String String)
    , clientSparkVersion :: String
    , mainClass :: String
    , environmentVariables :: Maybe (Map String String)
    , appArgs :: [String]
} deriving (Show,Generic)

instance FromJSON StandaloneParam
instance ToJSON StandaloneParam

data StandaloneConf = StandaloneConf{
  restHost :: String
  , standaloneParam :: StandaloneParam
} deriving (Show,Generic)

instance FromJSON StandaloneConf
instance ToJSON StandaloneConf

data Wrapper = Wrapper {
  standaloneConf :: StandaloneConf
} deriving (Show,Generic)

instance FromJSON Wrapper
instance ToJSON Wrapper

hardcodedConf :: StandaloneParam
hardcodedConf = standaloneParam where
  spm = M.fromList [ ("spark.executor.memory","2g")
                         , ("spark.master","spark://127.0.0.1:7077")
                         , ("spark.eventLog.enabled","false")
                         , ("spark.app.name","HARDCODED PI EXAMPLE")
                         , ("spark.jars","/opt/spark-2.4.0-bin-hadoop2.7/examples/jars/spark-examples_2.11-2.4.0.jar")
                         , ("spark.driver.supervise","true")]
  envVar = Just (fromList [("abc","aaa")])
  standaloneParam =  StandaloneParam{action = "CreateSubmissionRequest", appResource = "/opt/spark-2.4.0-bin-hadoop2.7/examples/jars/spark-examples_2.11-2.4.0.jar", mainClass = "org.apache.spark.examples.SparkPi" , sparkProperties = Just spm, clientSparkVersion =  "2.4.0", appArgs = ["80"], environmentVariables = envVar}

readConf :: MaybeT IO StandaloneConf
readConf = MaybeT $ do
    input <- B.readFile "standaloneConf.json"
    let mm = decode input :: Maybe Wrapper
    return (fmap (standaloneConf) mm)

evalConf :: Maybe StandaloneConf -> IO (StandaloneParam)
evalConf (Just conf) = do
  liftIO $ putStrLn ("Loaded StandaloneConf Successfully from the file standaloneConf.json")
  return (standaloneParam conf)
evalConf Nothing = do
  liftIO $ putStrLn ("FAILURE LOADING Conf from the file standaloneConf.json  ")
  return hardcodedConf
