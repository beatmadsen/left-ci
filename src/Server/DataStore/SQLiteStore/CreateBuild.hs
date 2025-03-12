{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore.CreateBuild (
  sqlCreateBuildUnlessExists,
  createEntitiesAt
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.String (fromString)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, field)
import Database.SQLite.Simple.QQ (sql)
import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.DataStore.SQLiteSetup
import Server.Domain (Build (..), BuildState (..), Version (..), Project (..))

import qualified Data.Map as Map

import Server.DataStore.SQLiteStore.BuildPair
import Server.DataStore.SQLiteStore.Types


sqlCreateBuildUnlessExists :: Project -> Version -> Build -> AtomicM OngoingTransaction (Either () ())
sqlCreateBuildUnlessExists project version build = do
  OngoingTransaction connection <- ask
  liftIO $ do
    alreadyExists <- doesBuildExist connection build
    if alreadyExists
      then return $ Left ()
      else do
        createEntities connection project version build
        return $ Right ()

doesBuildExist :: Connection -> Build -> IO Bool
doesBuildExist connection (Build buildId) = do
  results <-
    query
      connection
      "SELECT 1 FROM builds WHERE global_id = ?"
      (Only buildId) ::
      IO [Only Int]
  return $ not $ null results


createEntitiesAt :: Connection -> UTCTime -> Project -> Version -> Build -> IO ()
createEntitiesAt connection now project version build = do
  insertOrIgnoreProject connection project now
  insertOrIgnoreVersion connection project version now
  insertBuild connection version build now
  insertExecution Fast connection build now
  insertExecution Slow connection build now


createEntities :: Connection -> Project -> Version -> Build -> IO ()
createEntities connection project version buildId = do
  now <- getCurrentTime
  createEntitiesAt connection now project version buildId

insertOrIgnoreProject :: Connection -> Project -> UTCTime -> IO ()
insertOrIgnoreProject connection project now = do  
  execute
    connection
    "INSERT OR IGNORE INTO projects (name, created_at) VALUES (?, ?)"
    (project, now)

insertOrIgnoreVersion :: Connection -> Project -> Version -> UTCTime -> IO ()
insertOrIgnoreVersion connection project version now = do
  let Version commitHash = version
  execute
    connection
    [sql|
      INSERT OR IGNORE INTO versions (project_id, commit_hash, created_at)
      SELECT p.id, ?, ?
      FROM projects p
      WHERE p.name = ?
    |]
    (commitHash, now, project)

insertBuild :: Connection -> Version -> Build -> UTCTime -> IO ()
insertBuild connection (Version commitHash) buildId now = do
  let Build globalId = buildId
  execute
    connection
    [sql|
      INSERT INTO builds (version_id, global_id, created_at)
      SELECT v.id, ?, ?
      FROM versions v
      WHERE v.commit_hash = ?
    |]
    (globalId, now, commitHash)

insertExecution :: SuiteName -> Connection -> Build -> UTCTime -> IO ()
insertExecution suiteName connection (Build globalId) now = do
  execute
    connection
    [sql|
      INSERT INTO executions (suite_id, build_id, state, created_at, updated_at)
      SELECT s.id, b.id, ?, ?, ?
      FROM builds b, suites s
      WHERE b.global_id = ? AND s.name = ?
    |]
    (show Init, now, now, globalId, suiteName)