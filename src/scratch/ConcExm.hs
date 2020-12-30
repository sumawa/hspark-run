module ConcExm where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import System.IO

timeit :: IO a -> IO (a,Double)
timeit ioa = do
  t0 <- getCurrentTime
  a <- ioa
  t1 <- getCurrentTime
  return (a, realToFrac (t1 `diffUTCTime` t0))

worker :: Int -> IO Int
worker n = do
  hSetBuffering stdout NoBuffering
  print ("Executing n = " ++ (show n))
  threadDelay (10^2 * n)
  return (n*n)

test1 :: IO (Int,Int)
test1 = do
  val1 <- async $ worker 1000
  val2 <- async $ worker 2000
  (,) <$> wait val1 <*> wait val2

test2 :: IO (Either Int Int)
test2 = do
  hSetBuffering stdout NoBuffering
  let val1 = worker 1000
  let val2 = worker 2000
  race val1 val2

test3 :: IO [Int]
test3 = do
  hSetBuffering stdout NoBuffering
  mapConcurrently worker [0..1000]

main :: IO ()
main = do
  print =<< timeit test1
  print =<< timeit test2
  print =<< timeit test3

