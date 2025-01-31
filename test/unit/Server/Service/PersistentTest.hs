{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.Domain (BuildId (..), BuildState (..), BuildSummary (..), VersionId (..))
import Server.Service (BuildService (..), Outcome (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertBool, (@?=))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Server.DataStore.Atomic (AtomicM(..))

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows,
      TestLabel "given a store that reports build id already exists, createBuild reports conflict" testCreateBuildAlreadyExists,
      TestLabel "given a store that reports build id does not exist, createBuild reports success" testCreateBuildSuccess     
    ]

defaultAtomically :: AtomicM ctx a -> IO a
defaultAtomically action = 
  runReaderT (runAtomicM action) undefined

defaultStore :: BuildStore ctx
defaultStore = BuildStore {
  atomically = defaultAtomically,
  findBuildPair = undefined, 
  createBuildUnlessExists = undefined
  }

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
  let expected = Success
  actual @?= expected



defaultBuildPair :: BuildPair
defaultBuildPair = BuildPair {slowBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Init}, fastBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Running}}
