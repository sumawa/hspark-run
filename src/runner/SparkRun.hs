{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module SparkRun where

import Control.Monad.Trans.Except
import Control.Monad 
import Data.Aeson
import Data.Text
import GHC.Generics
import Control.Exception

import Control.Lens ((&), (^.), (^?), (.~))
import Data.Aeson (FromJSON,ToJSON)
import Data.Aeson.Types
import Data.Aeson.Lens (key)


import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
--Data.ByteString.Lazy.Char8
import Data.Typeable
import Network.Wreq as W


import StandaloneRun (StandaloneParam(..))

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

class SparkRun v where
  postApplication :: v -> ExceptT String IO SpResponse
  getApplication :: v -> ExceptT String IO SpResponse
  generateParam :: v -> SparkCommand ->  v

instance SparkRun (StandaloneParam) where
  postApplication :: StandaloneParam -> ExceptT String IO SpResponse
  postApplication sp = ExceptT $ do
    let opts = defaults
    result <- try (postWith opts "http://127.0.0.1:6066/v1/submissions/create" (encode sp)) :: IO (Either SomeException (Response B.ByteString))
    case result of
      Left ex  -> return (Left $ "Caught exception  : " ++ show ex)
      Right val -> return spResponse where
          resp = val ^? responseBody
          spResponse = case (join ( fmap (\x -> decode x :: Maybe SpResponse) resp )) of
            Just sp -> Right $ sp
            Nothing -> Left $ "Standalone Submit Failed for unknown reasons: "

  getApplication :: StandaloneParam -> ExceptT String IO SpResponse
  getApplication spm = ExceptT $ do
    let opts = defaults
    result <- try (get "http://127.0.0.1:6066/v1/submissions/status/asdfsdfsdf") :: IO (Either SomeException (Response B.ByteString))
    case result of
      Left ex  -> return (Left $ "Caught exception  : " ++ show ex)
      Right val -> return spResponse where
          resp = val ^? responseBody
          spResponse = case (join ( fmap (\x -> decode x :: Maybe SpResponse) resp )) of
            Just sp -> Right $ sp
            Nothing -> Left $ "Some Other problem"

  generateParam :: StandaloneParam -> SparkCommand ->  StandaloneParam
  generateParam spm sc = standaloneParam where
    appRes = applicationJar (sc)
    sparkCls = sparkClass (sc)
    standaloneParam = spm {mainClass = sparkCls, appResource = appRes}

