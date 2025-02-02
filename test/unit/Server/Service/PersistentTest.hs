{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.Domain (BuildId (..), BuildState (..), BuildSummary (..), VersionId (..))
import Server.Service (BuildService (..), CreationOutcome (..), StateChangeOutcome (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertBool, (@?=))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Server.DataStore.Atomic (AtomicM(..))
import Control.Monad.IO.Class (liftIO)

import Data.IORef

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows,
      TestLabel "given a store that reports build id already exists, createBuild reports conflict" testCreateBuildAlreadyExists,
      TestLabel "given a store that reports build id does not exist, createBuild reports success" testCreateBuildSuccess,
      TestLabel "given a store and a non-existent build id, advanceFastSuite reports NotFound" testAdvanceFastResultNonExistent,
      TestLabel "given a store that returns a fast state, and succeeds in updating it, advanceFastSuite reports SuccessfullyChangedState" testAdvanceFastResultSuccess,
      TestLabel "given a store that returns a fast state, advanceFastSuite advances and updates the fast state" testAdvanceFastResultAdvancesAndUpdates
    ]


testGetBuildSummaryNonExistent :: Test
testGetBuildSummaryNonExistent = TestCase $ do
  let service = makePersistentService defaultStore {findBuildPair = const $ pure Nothing}
  actual <- getBuildSummary service (BuildId "123")
  let expected = Nothing
  actual @?= expected

testGetBuildSummaryTwoRows :: Test
testGetBuildSummaryTwoRows = TestCase $ do
  let service =
        makePersistentService
          defaultStore
            { findBuildPair = const $ pure $ Just defaultBuildPair
            }
  actual <- getBuildSummary service (BuildId "123")
  let expected = Just $ BuildSummary {slowState = Init, fastState = Running}
  actual @?= expected

testCreateBuildAlreadyExists :: Test
testCreateBuildAlreadyExists = TestCase $ do
  let service = makePersistentService defaultStore {createBuildUnlessExists = (const . const) $ pure $ Left ()}
  actual <- createBuild service (VersionId "123") (BuildId "456")
  let expected = Conflict
  actual @?= expected

testCreateBuildSuccess :: Test
testCreateBuildSuccess = TestCase $ do
  let service = makePersistentService defaultStore {createBuildUnlessExists = (const . const) $ pure $ Right ()}
  actual <- createBuild service (VersionId "123") (BuildId "456")
  let expected = SuccessfullyCreated
  actual @?= expected

testAdvanceFastResultNonExistent :: Test
testAdvanceFastResultNonExistent = TestCase $ do
  let service = makePersistentService defaultStore { findFastState = const $ pure Nothing }
  actual <- advanceFastSuite service (BuildId "123")
  let expected = NotFound
  actual @?= expected

testAdvanceFastResultSuccess :: Test
testAdvanceFastResultSuccess = TestCase $ do
  let service = makePersistentService defaultStore { 
    findFastState = const $ pure $ Just Running,
    updateFastState = (const . const) $ pure () 
    }
  actual <- advanceFastSuite service (BuildId "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testAdvanceFastResultAdvancesAndUpdates :: Test
testAdvanceFastResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef Init

  let service = makePersistentService defaultStore { 
    findFastState = const $ pure $ Just Init,
    updateFastState = \buildId newState -> liftIO $ writeIORef writtenState newState
    }
  advanceFastSuite service (BuildId "123")
  actual <- readIORef writtenState
  let expected = Running
  actual @?= expected

defaultBuildPair :: BuildPair
defaultBuildPair = BuildPair {slowBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Init}, fastBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Running}}

defaultAtomically :: AtomicM ctx a -> IO a
defaultAtomically action = 
  runReaderT (runAtomicM action) undefined

defaultStore :: BuildStore ctx
defaultStore = BuildStore {
  atomically = defaultAtomically,
  findBuildPair = undefined, 
  createBuildUnlessExists = undefined,
  findFastState = undefined,
  updateFastState = undefined
  }