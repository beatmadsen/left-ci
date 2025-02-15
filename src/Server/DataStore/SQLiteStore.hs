{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteStore
  ( makeSQLiteBuildStore,
  )
where

import Control.Monad.Reader (local)
import Database.SQLite.Simple (Connection, Query (..), close, execute_, open)
import Server.DataStore (BuildPair (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.DataStore.SQLiteSetup
import Server.Domain (BuildId, BuildState, VersionId)

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
sqlFindBuildPair buildId = undefined

sqlCreateBuildUnlessExists :: BuildId -> VersionId -> AtomicM OngoingTransaction (Either () ())
sqlCreateBuildUnlessExists buildId versionId = undefined

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
beginTransaction connection = undefined

commitOngoingTransaction :: OngoingTransaction -> IO ()
commitOngoingTransaction ot = undefined
