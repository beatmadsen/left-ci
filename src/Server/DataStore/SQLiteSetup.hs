{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteSetup
  ( SQLiteClient,
    initSQLiteDatabase,
  )
where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToRow
import Server.DataStore.TmpDir
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Server.DataStore.SQLiteTypes (SuiteName (..))

-- A SQLiteClient is a function that can create fresh connections when needed
type SQLiteClient = IO Connection

initSQLiteDatabase :: FilePath -> IO (FilePath, FilePath, SQLiteClient)
initSQLiteDatabase subDir = do
  dbDir <- tmpDir subDir
  let dbPath = dbDir </> "left-ci.db"  
  initDb dbPath
  -- Return a function that creates fresh connections
  return (dbDir, dbPath, openConnection dbPath)

openConnection :: FilePath -> IO Connection
openConnection dbPath = do
  conn <- open dbPath
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"
  return conn

initDb :: FilePath -> IO ()
initDb dbPath = do
  conn <- openConnection dbPath
  initTables conn
  close conn

initTables :: Connection -> IO ()
initTables conn = do
  createVersionsTable conn
  createBuildsTable conn
  createAndPopulateSuitesTable conn
  createExecutionsTable conn

createVersionsTable :: Connection -> IO ()
createVersionsTable conn = do
  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS versions (
        id INTEGER PRIMARY KEY,
        commit_hash TEXT NOT NULL UNIQUE,
        created_at DATETIME NOT NULL
    )
  |]

createBuildsTable :: Connection -> IO ()
createBuildsTable conn = do
  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS builds (
        id INTEGER PRIMARY KEY,
        version_id INTEGER NOT NULL,
        global_id TEXT NOT NULL UNIQUE,
        created_at DATETIME NOT NULL,
        FOREIGN KEY (version_id) REFERENCES versions(id)
    )
  |]

createAndPopulateSuitesTable :: Connection -> IO ()
createAndPopulateSuitesTable conn = do
  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS suites (
        id INTEGER PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
    )
  |]
  execute conn [sql| 
    INSERT OR IGNORE INTO suites (id, name) 
    VALUES (100, ?), (200, ?)
   |] (Slow, Fast)

createExecutionsTable :: Connection -> IO ()
createExecutionsTable conn = do
  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS executions (
        id INTEGER PRIMARY KEY,
        build_id INTEGER NOT NULL,
        suite_id INTEGER NOT NULL,
        state TEXT NOT NULL,
        created_at DATETIME NOT NULL,
        updated_at DATETIME NOT NULL,
        FOREIGN KEY (build_id) REFERENCES builds(id),
        FOREIGN KEY (suite_id) REFERENCES suites(id),
        UNIQUE(build_id, suite_id)
    )
  |]
