module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import qualified Config.ValidatorTest ( tests )
main :: IO ()
main = do
    counts <- runTestTT $ TestList [Config.ValidatorTest.tests]
    putStrLn $ "Tests run: " ++ show (tried counts)
    putStrLn $ "Failures: " ++ show (failures counts)
    putStrLn $ "Errors: " ++ show (errors counts)
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess