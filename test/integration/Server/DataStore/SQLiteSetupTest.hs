module Server.DataStore.SQLiteSetupTest
  ( tests,
  )
where

import Test.HUnit
import Server.DataStore.SQLiteSetup


tests :: Test
tests =
  TestList
    [ TestLabel "Given x, when y, then z" testXyz]

testXyz :: Test
testXyz = TestCase $ undefined