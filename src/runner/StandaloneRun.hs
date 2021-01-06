{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
--  , generateStandaloneParam
--  , post_StandaloneSubmitE
--  , get_StandaloneSubmitExcept
  , readConf
  , evalConf
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
import qualified Data.ByteString.Lazy.Char8 as B
--Data.ByteString.Lazy.Char8
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Network.Wreq as W
--import SparkRun (SpResponse,SparkCommand(..))
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

--generateStandaloneParam :: StandaloneParam -> SparkCommand ->  StandaloneParam
--generateStandaloneParam spm sc = standaloneParam where
--  appRes = applicationJar (sc)
--  sparkCls = sparkClass (sc)
--  standaloneParam = spm {mainClass = sparkCls, appResource = appRes}

{-
  ExceptT usage
    https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#g:4

    Usage reasons
    https://stackoverflow.com/questions/53453944/what-purpose-does-the-complexity-of-except-serve-in-haskell
  ANTI PATTERNS:
    https://www.fpcomplete.com/haskell/tutorial/exceptions/
    https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/

  https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
  https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
  https://www.stackbuilders.com/news/errors-and-exceptions-in-haskell
  https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
-}

-- FIXME: read host port from standaloneConf.json 
--post_StandaloneSubmitE :: StandaloneParam -> ExceptT String IO SpResponse
--post_StandaloneSubmitE sp = ExceptT $ do
--  let opts = defaults
--  result <- try (postWith opts "http://127.0.0.1:6066/v1/submissions/create" (encode sp)) :: IO (Either SomeException (Response B.ByteString))
--  case result of
--    Left ex  -> return (Left $ "Caught exception  : " ++ show ex)
--    Right val -> return spResponse where
--        resp = val ^? responseBody
--        spResponse = case (join ( fmap (\x -> decode x :: Maybe SpResponse) resp )) of
--          Just sp -> Right $ sp
--          Nothing -> Left $ "Standalone Submit Failed for unknown reasons: "

{-
  Api call for tracking jobs
  http://127.0.0.1:6066/v1/submissions/status/asdfsdfsdf
  {
  "action" : "SubmissionStatusResponse",
  "serverSparkVersion" : "2.4.0",
  "submissionId" : "asdfsdfsdf",
  "success" : false
  }
-}
{-
  http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#g:4
-}
-- FIXME: read host port from standaloneConf.json
--get_StandaloneSubmitExcept :: ExceptT String IO SpResponse
--get_StandaloneSubmitExcept = ExceptT $ do
--  let opts = defaults
--  result <- try (get "http://127.0.0.1:6066/v1/submissions/status/asdfsdfsdf") :: IO (Either SomeException (Response B.ByteString))
--  case result of
--    Left ex  -> return (Left $ "Caught exception  : " ++ show ex)
--    Right val -> return spResponse where
--        resp = val ^? responseBody
--        spResponse = case (join ( fmap (\x -> decode x :: Maybe SpResponse) resp )) of
--          Just sp -> Right $ sp
--          Nothing -> Left $ "Some Other problem"

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
