module Server.DataStore.TmpDirTest 
( tests,
  )
where

import Test.HUnit
import System.Directory (doesDirectoryExist)
import Server.DataStore.TmpDir


tests :: Test
tests = TestList
    [ TestLabel "Given a subdirectory, when creating a tmpdir, then the subdirectory is also created" testCreation]

testCreation :: Test
testCreation = TestCase $ do
  path <- tmpDir "test-subdir"
  actual <- doesDirectoryExist path
  assertEqual "Subdirectory is created" True actual

