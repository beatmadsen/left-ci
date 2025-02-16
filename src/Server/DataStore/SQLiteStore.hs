{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore
  ( makeSQLiteBuildStore,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, local)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (FromRow, field)
import Database.SQLite.Simple.QQ (sql)
import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.DataStore.SQLiteSetup
import Server.Domain (BuildId (..), BuildState (..), VersionId (..))

newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }

makeSQLiteBuildStore :: FilePath -> IO (BuildStore OngoingTransaction)
makeSQLiteBuildStore subDir = do
  (dbDir, dbPath, client) <- initSQLiteDatabase subDir
  pure
    BuildStore
      { findBuildPair = sqlFindBuildPair,
        createBuildUnlessExists = sqlCreateBuildUnlessExists,
        atomically = sqlAtomically client,
        findFastState = sqlFindState "fast",
        updateFastState = sqlUpdateState "fast",
        findSlowState = sqlFindState "slow",
        updateSlowState = sqlUpdateState "slow"
      }

sqlAtomically :: SQLiteClient -> AtomicM OngoingTransaction a -> IO a
sqlAtomically client atomicAction = do
  connection <- client
  ot <- beginTransaction connection
  result <- executeAtomic atomicAction ot
  commitOngoingTransaction ot
  return result

sqlFindBuildPair :: BuildId -> AtomicM OngoingTransaction (Maybe BuildPair)
sqlFindBuildPair buildId = do
  OngoingTransaction connection <- ask
  executions <- liftIO $ findExecutions connection buildId
  return $ mapExecutions executions

data Execution = Execution
  { suiteName :: Text,
    buildGlobalId :: Text,
    versionCommitHash :: Text,
    executionState :: Text
  }

instance FromRow Execution where
  fromRow =
    Execution
      <$> field -- suite_name
      <*> field -- build_global_id
      <*> field -- version_commit_hash
      <*> field -- state

mapExecutions :: [Execution] -> Maybe BuildPair
mapExecutions executions =
  let allFast = [e | e <- executions, suiteName e == "fast"]
      allSlow = [e | e <- executions, suiteName e == "slow"]
   in case (allFast, allSlow) of
        ([], []) -> Nothing
        ([], _) -> error "No fast suite execution found"
        (_, []) -> error "No slow suite execution found"
        ([fast], [slow]) -> Just (BuildPair (mapExecution slow) (mapExecution fast))
        _ -> error "Multiple executions found for a single suite"

mapExecution :: Execution -> BuildRecord
mapExecution ex =
  BuildRecord
    (BuildId (buildGlobalId ex))
    (VersionId (versionCommitHash ex))
    (fromString . unpack . executionState $ ex)

findExecutions :: Connection -> BuildId -> IO [Execution]
findExecutions connection (BuildId buildId) =
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

sqlCreateBuildUnlessExists :: BuildId -> VersionId -> AtomicM OngoingTransaction (Either () ())
sqlCreateBuildUnlessExists buildId versionId = do
  OngoingTransaction connection <- ask
  liftIO $ do
    alreadyExists <- doesBuildExist connection buildId
    if alreadyExists
      then return $ Left ()
      else do
        createVersionAndBuildAndExecutions connection versionId buildId
        return $ Right ()

doesBuildExist :: Connection -> BuildId -> IO Bool
doesBuildExist connection (BuildId buildId) = do
  results <-
    query
      connection
      "SELECT 1 FROM builds WHERE global_id = ?"
      (Only buildId) ::
      IO [Only Int]
  return $ not $ null results

createVersionAndBuildAndExecutions :: Connection -> VersionId -> BuildId -> IO ()
createVersionAndBuildAndExecutions connection versionId buildId = do
  now <- getCurrentTime
  insertOrIgnoreVersion connection versionId now
  insertBuild connection versionId buildId now
  insertExecution "fast" connection buildId now
  insertExecution "slow" connection buildId now

insertOrIgnoreVersion :: Connection -> VersionId -> UTCTime -> IO ()
insertOrIgnoreVersion connection versionId now = do
  let VersionId commitHash = versionId
  execute
    connection
    "INSERT OR IGNORE INTO versions (commit_hash, created_at) VALUES (?, ?)"
    (commitHash, now)

insertBuild :: Connection -> VersionId -> BuildId -> UTCTime -> IO ()
insertBuild connection (VersionId commitHash) buildId now = do
  let BuildId globalId = buildId
  execute
    connection
    [sql|
      INSERT INTO builds (version_id, global_id, created_at)
      SELECT v.id, ?, ?
      FROM versions v
      WHERE v.commit_hash = ?
    |]
    (globalId, now, commitHash)

insertExecution :: String -> Connection -> BuildId -> UTCTime -> IO ()
insertExecution suiteName connection (BuildId globalId) now = do
  execute
    connection
    [sql|
      INSERT INTO executions (suite_id, build_id, state, created_at, updated_at)
      SELECT s.id, b.id, ?, ?, ?
      FROM builds b, suites s
      WHERE b.global_id = ? AND s.name = ?
    |]
    (show Init, now, now, globalId, suiteName)

sqlFindState :: String -> BuildId -> AtomicM OngoingTransaction (Maybe BuildState)
sqlFindState suiteName buildId = do
  OngoingTransaction connection <- ask
  results <- liftIO $ queryState suiteName connection buildId
  return $ takeFirstState results

queryState :: String -> Connection -> BuildId -> IO [Only String]
queryState suiteName connection (BuildId globalId) = 
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

sqlUpdateState :: String -> BuildId -> BuildState -> AtomicM OngoingTransaction ()
sqlUpdateState suiteName (BuildId globalId) state = do
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

-- helpers and ideas

beginTransaction :: Connection -> IO OngoingTransaction
beginTransaction connection = do
  execute_ connection "BEGIN TRANSACTION"
  return $ OngoingTransaction connection

commitOngoingTransaction :: OngoingTransaction -> IO ()
commitOngoingTransaction ot = do
  execute_ (connection ot) "COMMIT"
