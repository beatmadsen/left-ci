module Main where

import Config.ApplicationConfig (parseApplicationConfig)
import Config.Validator (validate)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    v <- validate $ parseApplicationConfig args
    putStrLn $ show v
