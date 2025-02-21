{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Data.IORef
import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM (..))
import Server.Domain (Build (..), BuildState (..), BuildSummary (..), Version (..), Project (..))
import Server.Service (BuildService (..), CreationOutcome (..), StateChangeOutcome (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertBool, (@?=))

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows,
      TestLabel "given a store that reports build id already exists, createBuild reports conflict" testCreateBuildAlreadyExists,
      TestLabel "given a store that reports build id does not exist, createBuild reports success" testCreateBuildSuccess,
      TestLabel "given a store and a non-existent build id, advanceFastSuite reports NotFound" testAdvanceFastResultNonExistent,
      TestLabel "given a store that returns a fast state, and succeeds in updating it, advanceFastSuite reports SuccessfullyChangedState" testAdvanceFastResultSuccess,
      TestLabel "given a store that returns a fast state, advanceFastSuite advances and updates the fast state" testAdvanceFastResultAdvancesAndUpdates,
      TestLabel "given a store that returns a slow state, and succeeds in updating it, advanceSlowSuite reports SuccessfullyChangedState" testAdvanceSlowResultSuccess,
      TestLabel "given a store that returns a slow state, advanceSlowSuite advances and updates the slow state" testAdvanceSlowResultAdvancesAndUpdates,
      TestLabel "given a store that returns a fast state, and succeeds in updating it, failFastSuite reports SuccessfullyChangedState" testFailFastResultSuccess,
      TestLabel "given a store that returns a fast state, failFastSuite advances and updates the fast state" testFailFastResultAdvancesAndUpdates,
      TestLabel "given a store that returns a slow state, and succeeds in updating it, failSlowSuite reports SuccessfullyChangedState" testFailSlowResultSuccess,
      TestLabel "given a store that returns a slow state, failSlowSuite advances and updates the slow state" testFailSlowResultAdvancesAndUpdates
    ]

testGetBuildSummaryNonExistent :: Test
testGetBuildSummaryNonExistent = TestCase $ do
  let service = makePersistentService defaultStore {findBuildPair = const $ pure Nothing}
  actual <- getBuildSummary service (Build "123")
  let expected = Nothing
  actual @?= expected

testGetBuildSummaryTwoRows :: Test
testGetBuildSummaryTwoRows = TestCase $ do
  let service =
        makePersistentService
          defaultStore
            { findBuildPair = const $ pure $ Just defaultBuildPair
            }
  actual <- getBuildSummary service (Build "123")
  let expected = Just $ BuildSummary {slowState = Init, fastState = Running}
  actual @?= expected

testCreateBuildAlreadyExists :: Test
testCreateBuildAlreadyExists = TestCase $ do
  let service = makePersistentService defaultStore {createBuildUnlessExists = (const . const . const) $ pure $ Left ()}
  actual <- createBuild service (Project "abc") (Version "123") (Build "456")
  let expected = Conflict
  actual @?= expected

testCreateBuildSuccess :: Test
testCreateBuildSuccess = TestCase $ do
  let service = makePersistentService defaultStore {createBuildUnlessExists = (const . const . const) $ pure $ Right ()}
  actual <- createBuild service (Project "abc") (Version "123") (Build "456")
  let expected = SuccessfullyCreated
  actual @?= expected

testAdvanceFastResultNonExistent :: Test
testAdvanceFastResultNonExistent = TestCase $ do
  let service = makePersistentService defaultStore {findFastState = const $ pure Nothing}
  actual <- advanceFastSuite service (Build "123")
  let expected = NotFound
  actual @?= expected

testAdvanceFastResultSuccess :: Test
testAdvanceFastResultSuccess = TestCase $ do
  let service = makeServiceWithFastStubs Running
  actual <- advanceFastSuite service (Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testAdvanceFastResultAdvancesAndUpdates :: Test
testAdvanceFastResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef Init
  let service = makeServiceWithFastMocks writtenState
  advanceFastSuite service (Build "123")
  actual <- readIORef writtenState
  let expected = Running
  actual @?= expected

testAdvanceSlowResultSuccess :: Test
testAdvanceSlowResultSuccess = TestCase $ do
  let service = makeServiceWithSlowStubs Init
  actual <- advanceSlowSuite service (Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testAdvanceSlowResultAdvancesAndUpdates :: Test
testAdvanceSlowResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef Init
  let service = makeServiceWithSlowMocks writtenState
  advanceSlowSuite service (Build "123")
  actual <- readIORef writtenState
  let expected = Running
  actual @?= expected

testFailFastResultSuccess :: Test
testFailFastResultSuccess = TestCase $ do
  let service = makeServiceWithFastStubs Running
  actual <- failFastSuite service (Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testFailFastResultAdvancesAndUpdates :: Test
testFailFastResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef Running
  let service = makeServiceWithFastMocks writtenState
  failFastSuite service (Build "123")
  actual <- readIORef writtenState
  let expected = Failed
  actual @?= expected

testFailSlowResultSuccess :: Test
testFailSlowResultSuccess = TestCase $ do
  let service = makeServiceWithSlowStubs Running
  actual <- failSlowSuite service (Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testFailSlowResultAdvancesAndUpdates :: Test
testFailSlowResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef Running
  let service = makeServiceWithSlowMocks writtenState
  failSlowSuite service (Build "123")
  actual <- readIORef writtenState
  let expected = Failed
  actual @?= expected

makeServiceWithFastStubs :: BuildState -> BuildService
makeServiceWithFastStubs inital =
  makePersistentService
    defaultStore
      { findFastState = makeStubbedFind inital,
        updateFastState = makeStubbedUpdate
      }

makeServiceWithSlowStubs :: BuildState -> BuildService
makeServiceWithSlowStubs inital =
  makePersistentService
    defaultStore
      { findSlowState = makeStubbedFind inital,
        updateSlowState = makeStubbedUpdate
      }

makeStubbedFind :: BuildState -> (Build -> AtomicM ctx (Maybe BuildState))
makeStubbedFind state = const $ pure $ Just state

makeStubbedUpdate :: (Build -> BuildState -> AtomicM ctx ())
makeStubbedUpdate = (const . const) $ pure ()

makeMockedFind :: IORef BuildState -> (Build -> AtomicM ctx (Maybe BuildState))
makeMockedFind ref = const $ fmap Just $ liftIO $ readIORef ref

makeMockedUpdate :: IORef BuildState -> (Build -> BuildState -> AtomicM ctx ())
makeMockedUpdate ref = \_ newState -> liftIO $ writeIORef ref newState

makeServiceWithFastMocks :: IORef BuildState -> BuildService
makeServiceWithFastMocks ref =
  makePersistentService
    defaultStore
      { findFastState = makeMockedFind ref,
        updateFastState = makeMockedUpdate ref
      }

makeServiceWithSlowMocks :: IORef BuildState -> BuildService
makeServiceWithSlowMocks ref =
  makePersistentService
    defaultStore
      { findSlowState = makeMockedFind ref,
        updateSlowState = makeMockedUpdate ref
      }

defaultBuildPair :: BuildPair
defaultBuildPair = BuildPair {slowSuite = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Init}, fastSuite = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Running}}

defaultAtomically :: AtomicM ctx a -> IO a
defaultAtomically action =
  runReaderT (runAtomicM action) undefined

defaultStore :: BuildStore ctx
defaultStore =
  BuildStore
    { atomically = defaultAtomically,
      findBuildPair = undefined,
      createBuildUnlessExists = undefined,
      findFastState = undefined,
      updateFastState = undefined,
      findSlowState = undefined,
      updateSlowState = undefined
    }
