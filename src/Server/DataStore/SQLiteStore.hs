{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore
  ( makeSQLiteBuildStore,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, local)
import Data.String (fromString)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, field)
import Database.SQLite.Simple.QQ (sql)
import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.DataStore.SQLiteSetup
import Server.Domain (Build (..), BuildState (..), Version (..), Project (..))
import Server.DataStore.SQLiteTypes

newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }

makeSQLiteBuildStore :: FilePath -> IO (BuildStore OngoingTransaction)
makeSQLiteBuildStore subDir = do
  (dbDir, dbPath, client) <- initSQLiteDatabase subDir
  pure
    BuildStore
      { findBuildPair = sqlFindBuildPair,
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

sqlFindBuildPair :: Build -> AtomicM OngoingTransaction (Maybe BuildPair)
sqlFindBuildPair buildId = do
  OngoingTransaction connection <- ask
  executions <- liftIO $ findExecutions connection buildId
  return $ mapExecutions executions

sqlFindBuildPairs :: Project -> AtomicM OngoingTransaction [BuildPair]
sqlFindBuildPairs project = undefined

mapExecutions :: [Execution] -> Maybe BuildPair
mapExecutions executions =
  let allFast = [e | e <- executions, suiteName e == Fast]
      allSlow = [e | e <- executions, suiteName e == Slow]
   in case (allFast, allSlow) of
        ([], []) -> Nothing
        ([], _) -> error "No fast suite execution found"
        (_, []) -> error "No slow suite execution found"
        ([fast], [slow]) -> Just (BuildPair (mapExecution slow) (mapExecution fast))
        _ -> error "Multiple executions found for a single suite"

mapExecution :: Execution -> BuildRecord
mapExecution ex =
  BuildRecord
    (buildGlobalId ex)
    (versionCommitHash ex)
    (executionState ex)

findExecutions :: Connection -> Build -> IO [Execution]
findExecutions connection (Build buildId) =
  query
    connection
    [sql|
      SELECT s.name, b.global_id, v.commit_hash, e.state 
      FROM executions e 
      JOIN builds b ON e.build_id = b.id 
      JOIN versions v ON b.version_id = v.id
      JOIN suites s ON e.suite_id = s.id
      WHERE b.global_id = ?
      |]
    (Only buildId)

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

createEntities :: Connection -> Project -> Version -> Build -> IO ()
createEntities connection project version buildId = do
  now <- getCurrentTime
  insertOrIgnoreProject connection project now
  insertOrIgnoreVersion connection project version now
  insertBuild connection version buildId now
  insertExecution Fast connection buildId now
  insertExecution Slow connection buildId now

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

sqlFindState :: SuiteName -> Build -> AtomicM OngoingTransaction (Maybe BuildState)
sqlFindState suiteName buildId = do
  OngoingTransaction connection <- ask
  results <- liftIO $ queryState suiteName connection buildId
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
    execute
      connection
      [sql|
        UPDATE executions
        SET state = ?
        WHERE suite_id IN (SELECT id FROM suites WHERE name = ?)
        AND build_id IN (SELECT id FROM builds WHERE global_id = ?)
      |]
      (show state, suiteName, globalId)

beginTransaction :: Connection -> IO OngoingTransaction
beginTransaction connection = do
  execute_ connection "BEGIN TRANSACTION"
  return $ OngoingTransaction connection

commitOngoingTransaction :: OngoingTransaction -> IO ()
commitOngoingTransaction ot = do
  execute_ (connection ot) "COMMIT"
