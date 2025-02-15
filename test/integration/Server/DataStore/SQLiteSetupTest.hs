{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteSetupTest
  ( tests,
  )
where

import Test.HUnit
import Server.DataStore.SQLiteSetup (initSQLiteDatabase)
import Control.Exception (bracket)
import System.Directory (removeDirectoryRecursive, doesFileExist)
import Database.SQLite.Simple (close, execute_)



tests :: Test
tests =
  TestList
    [ TestLabel "Given an initialised database, when querying build table, then it is empty" testXyz]

testXyz :: Test
testXyz = TestCase $ bracket
  (initSQLiteDatabase "test-123") -- setup
  (\(dbDir, dbPath, client) -> do    
    -- teardown
    removeDirectoryRecursive dbDir
  )
  (\(dbDir, dbPath, client) -> do
    -- test
    connection <- client
    execute_ connection "SELECT count(*) FROM build"
    close connection
    return ()
  )  
  
