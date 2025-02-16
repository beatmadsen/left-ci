{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore
  ( makeSQLiteBuildStore,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, local)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ (sql)
import Server.DataStore (BuildPair (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.DataStore.SQLiteSetup
import Server.Domain (BuildId (..), BuildState (..), VersionId (..))

newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }

type BuildKey = Int

type VersionKey = Int

makeSQLiteBuildStore :: FilePath -> IO (BuildStore OngoingTransaction)
makeSQLiteBuildStore subDir = do
  (dbDir, dbPath, client) <- initSQLiteDatabase subDir
  pure
    BuildStore
      { findBuildPair = sqlFindBuildPair,
        createBuildUnlessExists = sqlCreateBuildUnlessExists,
        atomically = sqlAtomically client,
        findFastState = sqlFindFastState,
        updateFastState = sqlUpdateFastState,
        findSlowState = sqlFindSlowState,
        updateSlowState = sqlUpdateSlowState
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
  liftIO $ do
    executions <- findExecutions connection buildId
    return Nothing

data Execution = Execution
  { suiteId :: Int,
    buildId :: Int,
    versionId :: Int,
    state :: String
  }

instance FromRow Execution where
  fromRow =
    Execution
      <$> field -- suite_id
      <*> field -- build_id
      <*> field -- version_id
      <*> field -- state

findExecutions :: Connection -> BuildId -> IO [Execution]
findExecutions connection (BuildId buildId) =
  query
    connection
    [sql|
      SELECT e.suite_id, e.build_id, b.version_id, e.state 
      FROM executions e 
      JOIN builds b ON e.build_id = b.id 
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
  versionKey <- insertOrIgnoreVersion connection versionId now
  buildKey <- insertBuild connection versionKey buildId now
  insertFastExecution connection buildKey now
  insertSlowExecution connection buildKey now

insertOrIgnoreVersion :: Connection -> VersionId -> UTCTime -> IO VersionKey
insertOrIgnoreVersion connection versionId now = do
  let VersionId vId = versionId
  execute
    connection
    "INSERT OR IGNORE INTO versions (commit_hash, created_at) VALUES (?, ?)"
    (vId, now)
  getVersionKey connection versionId

insertBuild :: Connection -> VersionKey -> BuildId -> UTCTime -> IO BuildKey
insertBuild connection versionKey buildId now = do
  let BuildId bId = buildId
  execute
    connection
    "INSERT INTO builds (version_id, global_id, created_at) VALUES (?, ?, ?)"
    (versionKey, bId, now)
  getBuildKey connection buildId

getVersionKey :: Connection -> VersionId -> IO VersionKey
getVersionKey connection (VersionId versionId) = do
  [Only versionKey] <-
    query
      connection
      "SELECT id FROM versions WHERE commit_hash = ?"
      (Only versionId)
  return versionKey

getBuildKey :: Connection -> BuildId -> IO BuildKey
getBuildKey connection (BuildId buildId) = do
  [Only buildKey] <-
    query
      connection
      "SELECT id FROM builds WHERE global_id = ?"
      (Only buildId)
  return buildKey

insertFastExecution :: Connection -> BuildKey -> UTCTime -> IO ()
insertFastExecution connection buildKey now = do
  execute
    connection
    "INSERT INTO executions (suite_id, build_id, state, created_at, updated_at) VALUES (200, ?, ?, ?, ?)"
    (buildKey, (show Init), now, now)

insertSlowExecution :: Connection -> BuildKey -> UTCTime -> IO ()
insertSlowExecution connection buildKey now = do
  execute
    connection
    "INSERT INTO executions (suite_id, build_id, state, created_at, updated_at) VALUES (100, ?, ?, ?, ?)"
    (buildKey, (show Init), now, now)

sqlFindFastState :: BuildId -> AtomicM OngoingTransaction (Maybe BuildState)
sqlFindFastState buildId = undefined

sqlUpdateFastState :: BuildId -> BuildState -> AtomicM OngoingTransaction ()
sqlUpdateFastState buildId state = undefined

sqlFindSlowState :: BuildId -> AtomicM OngoingTransaction (Maybe BuildState)
sqlFindSlowState buildId = undefined

sqlUpdateSlowState :: BuildId -> BuildState -> AtomicM OngoingTransaction ()
sqlUpdateSlowState buildId state = undefined

-- helpers and ideas

beginTransaction :: Connection -> IO OngoingTransaction
beginTransaction connection = do
  execute_ connection "BEGIN TRANSACTION"
  return $ OngoingTransaction connection

commitOngoingTransaction :: OngoingTransaction -> IO ()
commitOngoingTransaction ot = do
  execute_ (connection ot) "COMMIT"
