module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import qualified Config.ApplicationConfigTest ( tests )
import qualified Server.Service.PersistentTest ( tests )
import qualified Server.ServiceTest (tests)
main :: IO ()
main = do
    counts <- runTestTT $ TestList [
        Config.ApplicationConfigTest.tests,
        Server.Service.PersistentTest.tests,
        Server.ServiceTest.tests
        ]
    putStrLn $ "Tests run: " ++ show (tried counts)
    putStrLn $ "Failures: " ++ show (failures counts)
    putStrLn $ "Errors: " ++ show (errors counts)
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess