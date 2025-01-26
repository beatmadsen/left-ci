{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Server.DataStore (BuildStore (..))
import Server.Domain (BuildId (..), BuildState (..), BuildSummary (..))
import Server.Service (BuildService (..))
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=))

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a missing build id, getBuildSummary returns Nothing" $ TestCase $ do
        let store = BuildStore {demo = const "demo"}
        let service = makePersistentService store
        actual <- getBuildSummary service (BuildId "123")
        let expected = Nothing
        actual @?= expected
    ]