module Main where

import Config.ApplicationConfig (parseApplicationConfig, ApplicationConfig (..))
import Config.Validator (validate)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  v <- validate $ parseApplicationConfig args
  case v of
    Server port -> runServer port
    _ -> putStrLn $ show v

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Running server on port " ++ show port
  -- runServer port
