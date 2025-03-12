{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.DataStore.SQLiteStore.BuildPair (
  sqlFindBuildPair,
  sqlFindBuildPairs
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
import Server.Domain (Build (..), Project (..))
import qualified Data.Map as Map
import Server.DataStore.SQLiteStore.Types

sqlFindBuildPair :: Build -> AtomicM OngoingTransaction (Maybe BuildPair)
sqlFindBuildPair buildId = do
  OngoingTransaction connection <- ask
  executions <- liftIO $ findExecutions connection buildId
  return $ mapExecutionsM executions


sqlFindBuildPairs :: Project -> Maybe UTCTime -> AtomicM OngoingTransaction [BuildPair]
sqlFindBuildPairs project mAfter = do
  OngoingTransaction connection <- ask
  executions <- liftIO $ findProjectExecutions connection project mAfter
  return $ mapExecutions executions

findProjectExecutions :: Connection -> Project -> Maybe UTCTime -> IO [Execution]
findProjectExecutions connection project Nothing = do
  query connection [sql|
    SELECT s.name, b.global_id, v.commit_hash, e.state, e.created_at, e.updated_at
    FROM executions e
    JOIN builds b ON e.build_id = b.id
    JOIN versions v ON b.version_id = v.id
    JOIN suites s ON e.suite_id = s.id
    JOIN projects p ON v.project_id = p.id
    WHERE p.name = ?
    ORDER BY e.updated_at DESC
    LIMIT 20
  |] (Only project)

findProjectExecutions connection project (Just after) = do
  query connection [sql|
    SELECT s.name, b.global_id, v.commit_hash, e.state, e.created_at, e.updated_at
    FROM executions e
    JOIN builds b ON e.build_id = b.id
    JOIN versions v ON b.version_id = v.id
    JOIN suites s ON e.suite_id = s.id
    JOIN projects p ON v.project_id = p.id
    WHERE p.name = ? AND e.updated_at > ?
    ORDER BY e.updated_at DESC
    LIMIT 20
  |] (project, after)


mapExecutions :: [Execution] -> [BuildPair]
mapExecutions executions = do
  let grouped = groupByBuild executions
  execGroup <- Map.elems grouped
  return $ slowAndFast execGroup

slowAndFast :: [Execution] -> BuildPair
slowAndFast executions =
  case mapExecutionsM executions of
    Just buildPair -> buildPair
    Nothing -> error "No fast suite execution found"

groupByBuild :: [Execution] -> Map.Map Build [Execution]
groupByBuild executions =
  Map.fromListWith (++) [(buildGlobalId e, [e]) | e <- executions]

mapExecutionsM :: [Execution] -> Maybe BuildPair
mapExecutionsM executions =
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
    (executionCreatedAt ex)
    (executionUpdatedAt ex)

findExecutions :: Connection -> Build -> IO [Execution]
findExecutions connection (Build buildId) =
  query
    connection
    [sql|
      SELECT s.name, b.global_id, v.commit_hash, e.state, e.created_at, e.updated_at
      FROM executions e 
      JOIN builds b ON e.build_id = b.id 
      JOIN versions v ON b.version_id = v.id
      JOIN suites s ON e.suite_id = s.id
      WHERE b.global_id = ?
      |]
    (Only buildId)
