module ReaderEx where

{-
  In large programs where a configuration value is required at multiple places.
  We cannot have a global variable in pure function programs.
  So we pass these variables through functions.
  In larger programs, these “pass-through” variables can cause a lot of headaches.

  The Reader monad solves this problem.
    - It effectively creates a global read-only value of a specified type.
    - All functions within the monad can “read” the type.

-}
import Control.Monad.Trans.Reader

data Environment = Environment { param1 :: String, param2 :: String } deriving (Show,Eq)

loadEnv :: IO Environment
loadEnv = return (Environment "P1Val" "P2Val" )

withoutReaderMain :: IO ()
withoutReaderMain = do
  env <- loadEnv
  let str = func1 env
  print str

func1 :: Environment -> String
func1 env = (show (func2 env)) ++ " func1 res"

func2 :: Environment -> Int
func2 env = 2 + floor (func3 env)

func3 :: Environment -> Float
func3 env = 0.5

{-
  Improving above example using reader
-}
readerMain :: IO ()
readerMain = do
  env <- loadEnv
  let str = runReader func1Reader env
  print str

func1Reader :: Reader Environment String
func1Reader = do
  res <- func2Reader
  return ("func1 result : " ++ (show res))

func2Reader :: Reader Environment Int
func2Reader = do
  env <- ask
  let res3 = func3 env
  return (2 + floor (res3))

func3Reader :: Reader Environment Float
func3Reader = return 0.5
