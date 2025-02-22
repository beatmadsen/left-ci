module Server.DataStore.TmpDirTest
  ( tests,
  )
where

import Control.Exception (bracket)
import Server.DataStore.TmpDir
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, doesFileExist)
import Test.HUnit
import System.FilePath ((</>))
import RandomHelper (getUniqueDirName)

tests :: Test
tests =
  TestList
    [ TestLabel "Given a subdirectory, when creating a tmpdir, then the subdirectory is also created" testCreation,
      TestLabel "Given a subdirectory, when creating a tmpdir twice, then the operation is idempotent" testIdempotency,
      TestLabel "Given a tmpdir, we can create a file in it" testFileCreation
    ]

testCreation :: Test
testCreation =
  TestCase $
    bracket
      (getUniqueDirName >>= tmpDir) -- setup, outputs path
      removeDirectoryRecursive -- teardown, removes path from setup
      ( \path -> do
          -- test, uses the path from setup
          actual <- doesDirectoryExist path
          assertEqual "Subdirectory is created" True actual
      )

testIdempotency :: Test
testIdempotency =
  TestCase $
    bracket
      getDirTwice -- setup, outputs path
      removeDirectoryRecursive -- teardown, removes path from setup
      ( \path -> do
          -- test, uses the path from setup
          actual <- doesDirectoryExist path
          assertEqual "Subdirectory is created" True actual
      )
  where
    getDirTwice = do
      sameSubDir <- getUniqueDirName
      tmpDir sameSubDir
      tmpDir sameSubDir

testFileCreation :: Test
testFileCreation = TestCase $ bracket
  (getUniqueDirName >>= tmpDir) -- setup, outputs path
  removeDirectoryRecursive -- teardown, removes path from setup
  (\path -> do
    let filePath = path </> "test.txt"
    createFile filePath
    actual <- doesFileExist filePath
    assertEqual "File is created" True actual
  )

createFile :: FilePath -> IO ()
createFile path = do
  writeFile path "test"

