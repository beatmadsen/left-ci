{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore
  ( makeSQLiteBuildStore,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.String (fromString)
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
import Server.DataStore.SQLiteStore.CreateBuild
import Data.Time.Clock (UTCTime, getCurrentTime)



makeSQLiteBuildStore :: FilePath -> IO (BuildStore OngoingTransaction)
makeSQLiteBuildStore subDir = do
  (dbDir, dbPath, client) <- initSQLiteDatabase subDir
  pure
    BuildStore
      { findBuildPair = sqlFindBuildPair,
        findProject = sqlFindProject,
        findBuildPairs = sqlFindBuildPairs,
        createBuildUnlessExists = sqlCreateBuildUnlessExists,
        atomically = sqlAtomically client,
        findFastState = sqlFindState Fast,
        updateFastState = sqlUpdateState Fast,
        findSlowState = sqlFindState Slow,
        updateSlowState = sqlUpdateState Slow
      }

sqlAtomically :: SQLiteClient -> AtomicM OngoingTransaction a -> IO a
sqlAtomically client atomicAction = do
  connection <- client
  ot <- beginTransaction connection
  result <- executeAtomic atomicAction ot
  commitOngoingTransaction ot
  return result

sqlFindProject :: Project -> AtomicM OngoingTransaction (Maybe Project)
sqlFindProject project = do
  OngoingTransaction connection <- ask
  liftIO $ do
    results <- query connection "SELECT name FROM projects WHERE name = ?" (Only project)
    return $ case results of
      [] -> Nothing
      (Only project) : _ -> Just project


sqlFindState :: SuiteName -> Build -> AtomicM OngoingTransaction (Maybe BuildState)
sqlFindState suiteName build = do
  OngoingTransaction connection <- ask
  results <- liftIO $ queryState suiteName connection build
  return $ takeFirstState results

queryState :: SuiteName -> Connection -> Build -> IO [Only String]
queryState suiteName connection (Build globalId) = 
  query 
    connection
    [sql| 
      SELECT e.state 
      FROM executions e 
      JOIN builds b ON e.build_id = b.id
      JOIN suites s ON e.suite_id = s.id
      WHERE b.global_id = ? AND s.name = ?
    |]
    (globalId, suiteName) :: IO [Only String]

takeFirstState :: [Only String] -> Maybe BuildState
takeFirstState results = case results of
  [] -> Nothing
  (Only state) : _ -> Just $ fromString state

sqlUpdateState :: SuiteName -> Build -> BuildState -> AtomicM OngoingTransaction ()
sqlUpdateState suiteName (Build globalId) state = do
  OngoingTransaction connection <- ask  
  liftIO $ do
    now <- getCurrentTime
    execute
      connection
      [sql|
        UPDATE executions
        SET state = ?, updated_at = ?
        WHERE suite_id IN (SELECT id FROM suites WHERE name = ?)
        AND build_id IN (SELECT id FROM builds WHERE global_id = ?)
      |]
      (show state, now, suiteName, globalId)

beginTransaction :: Connection -> IO OngoingTransaction
beginTransaction connection = do
  execute_ connection "BEGIN TRANSACTION"
  return $ OngoingTransaction connection

commitOngoingTransaction :: OngoingTransaction -> IO ()
commitOngoingTransaction ot = do
  execute_ (connection ot) "COMMIT"
