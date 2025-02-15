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
import RandomHelper (getUniqueDirName)


tests :: Test
tests =
  TestList
    [ TestLabel "Given an initialised database, we can insert records" testInserts]

testInserts :: Test
testInserts = TestCase $ bracket
  (getUniqueDirName >>= initSQLiteDatabase) -- setup
  (\(dbDir, dbPath, client) -> do    
    -- teardown
    removeDirectoryRecursive dbDir
  )
  (\(dbDir, dbPath, client) -> do
    -- test
    connection <- client
    execute_ connection "INSERT INTO versions (commit_hash, created_at) VALUES ('abcd1234', '2021-01-01')"
    close connection
  )  
  