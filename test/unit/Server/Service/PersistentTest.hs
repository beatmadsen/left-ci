{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Server.DataStore (BuildRow (..), BuildStore (..))
import Server.Domain (BuildId (..), BuildState (..), BuildSummary (..))
import Server.Service (BuildService (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=))

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows
    ]

defaultStore :: BuildStore
defaultStore = BuildStore {getBuildRows = undefined}

testGetBuildSummaryNonExistent :: Test
testGetBuildSummaryNonExistent = TestCase $ do
  let service = makePersistentService defaultStore
  actual <- getBuildSummary service (BuildId "123")
  let expected = Nothing
  actual @?= expected

testGetBuildSummaryTwoRows :: Test
testGetBuildSummaryTwoRows = TestCase $ do
  let service =
        makePersistentService
          defaultStore
            { getBuildRows =
                const $
                  pure
                    [ BuildRow {buildId = "123", versionId = "04a66b1n", cadence = "slow", state = "init"},
                      BuildRow {buildId = "123", versionId = "04a66b1n", cadence = "fast", state = "running"}
                    ]
            }
  actual <- getBuildSummary service (BuildId "123")
  let expected = Just $ BuildSummary {slowState = Init, fastState = Running}
  actual @?= expected
