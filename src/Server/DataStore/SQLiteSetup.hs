{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteSetup (
  SQLiteClient,
  initSQLiteDatabase
) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Server.DataStore.TmpDir
import System.FilePath ((</>))

import System.Directory (removeDirectoryRecursive, doesFileExist)

-- A SQLiteClient is a function that can create fresh connections when needed
type SQLiteClient = IO Connection

initSQLiteDatabase :: FilePath -> IO (FilePath, FilePath, SQLiteClient)
initSQLiteDatabase subDir = do
  -- First, create an initial connection to set up the database
  dbDir <- tmpDir subDir
  let dbPath = dbDir </> "left-ci.db"
  conn <- openConnection dbPath 
  
  -- Create tables and set up initial schema
  execute_ conn "CREATE TABLE IF NOT EXISTS build (id INTEGER PRIMARY KEY)"
  
  close conn
  -- Return a function that creates fresh connections
  return (dbDir, dbPath, openConnection dbPath)

openConnection :: FilePath -> IO Connection
openConnection dbPath = do
  conn <- open dbPath
  putStrLn $ "Opened connection in client to: " ++ dbPath
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA journal_mode = WAL"
  return conn