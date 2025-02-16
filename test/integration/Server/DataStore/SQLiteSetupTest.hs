{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteSetupTest
  ( tests,
  )
where

import Control.Exception (bracket)
import Database.SQLite.Simple (close, execute_)
import RandomHelper (getUniqueDirName)
import Server.DataStore.SQLiteSetup (initSQLiteDatabase)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import Test.HUnit

tests :: Test
tests =
  TestList
    [TestLabel "Given an initialised database, we can insert records" testInserts]

testInserts :: Test
testInserts =
  TestCase $
    bracket
      -- setup
      ( do
          subDir <- getUniqueDirName
          (dbDir, dbPath, client) <- initSQLiteDatabase subDir
          initSQLiteDatabase subDir -- check idempotency
          return (dbDir, dbPath, client)
      ) 
      -- teardown
      ( \(dbDir, dbPath, client) -> do
          removeDirectoryRecursive dbDir
      )
      -- test
      ( \(dbDir, dbPath, client) -> do
          connection <- client
          execute_ connection "INSERT INTO versions (commit_hash, created_at) VALUES ('abcd1234', '2021-01-01')"
          execute_ connection "INSERT INTO builds (version_id, global_id, created_at) VALUES (1, 'global1', '2021-01-01')"
          execute_ connection "INSERT INTO executions (build_id, suite_id, state, created_at, updated_at) VALUES (1, 100, 'Init', '2021-01-01', '2021-01-01')"
          close connection
      )