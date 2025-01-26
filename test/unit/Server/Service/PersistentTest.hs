{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.Domain (BuildId (..), BuildState (..), BuildSummary (..))
import Server.Service (BuildService (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=), assertBool)

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows,
      TestLabel "given a store and existing version and build ids, createBuild reports conflict" testCreateBuildConflict
    ]

defaultStore :: BuildStore
defaultStore = BuildStore {findBuildPair = undefined}

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

testCreateBuildConflict :: Test
testCreateBuildConflict = TestCase $ do
  assertBool "TODO" True -- TODO

defaultBuildPair :: BuildPair
defaultBuildPair = BuildPair {slowBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Init}, fastBuild = BuildRecord {buildId = "123", versionId = "04a66b1n", state = Running}}

