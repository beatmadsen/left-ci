module Server.DataStore.SQLiteBuildStore
  (
  -- We'll add exports here as we implement them
  )
where

import Control.Monad.Reader (local)
import Data.Pool (Pool, takeResource)
import Database.SQLite.Simple (Connection)
import Server.DataStore (BuildPair (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..), executeAtomic)
import Server.Domain (BuildId, BuildState, VersionId)

newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }

makeSQLiteBuildStore :: IO (BuildStore OngoingTransaction)
makeSQLiteBuildStore = do
  pool <- newPool
  pure
    BuildStore
      { findBuildPair = sqlFindBuildPair,
        createBuildUnlessExists = sqlCreateBuildUnlessExists,
        atomically = sqlAtomically pool,
        findFastState = sqlFindFastState,
        updateFastState = sqlUpdateFastState,
        findSlowState = sqlFindSlowState,
        updateSlowState = sqlUpdateSlowState
      }

sqlAtomically :: Pool Connection -> AtomicM OngoingTransaction a -> IO a
sqlAtomically pool atomicAction = do
  ot <- makeOngoingTransaction pool
  result <- executeAtomic atomicAction ot
  commitOngoingTransaction pool ot
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

newPool :: IO (Pool Connection)
newPool = undefined

checkoutConnection :: Pool Connection -> IO Connection
checkoutConnection = undefined

beginTransaction :: Connection -> IO OngoingTransaction
beginTransaction connection = undefined

makeOngoingTransaction :: Pool Connection -> IO OngoingTransaction
makeOngoingTransaction pool = do
  connection <- checkoutConnection pool
  beginTransaction connection

commitOngoingTransaction :: Pool Connection -> OngoingTransaction -> IO ()
commitOngoingTransaction pool = undefined
