module Main where

import Test.HUnit
    ( runTestTT, Counts(failures, tried, errors), Test(TestList) )
import ServerTest ( serverTests )
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    counts <- runTestTT $ TestList [serverTests]
    putStrLn $ "Tests run: " ++ show (tried counts)
    putStrLn $ "Failures: " ++ show (failures counts)
    putStrLn $ "Errors: " ++ show (errors counts)
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess
