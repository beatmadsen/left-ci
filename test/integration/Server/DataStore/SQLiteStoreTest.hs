{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteStoreTest
  ( tests,
  )
where

import Test.HUnit
import Server.DataStore.SQLiteStore (makeSQLiteBuildStore)
import Control.Exception (bracket)
import System.Directory (removeDirectoryRecursive, doesFileExist)
import Database.SQLite.Simple (close, execute_)
import RandomHelper (getUniqueDirName)
import Control.Monad (when)
import Server.DataStore
import Server.Domain (BuildId(..), VersionId(..), BuildState(..))
import Server.DataStore (BuildPair(..), BuildRecord(..))

tests :: Test
tests =
  TestList
    [ TestLabel "Given a created build, when querying, then we can find complete build records for fast and slow suites" testBuildCreation,
      TestLabel "Given a created build, after finding a fast execution, then we can update it" testUpdateFastExecution
    ]

testBuildCreation :: Test
testBuildCreation = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    maybeBuildPair <- atomically buildStore $ findBuildPair buildStore (BuildId "build1")

    let expected = (Just (BuildPair (BuildRecord (BuildId "build1") (VersionId "version1") Init) (BuildRecord (BuildId "build1") (VersionId "version1") Init)))
    
    assertEqual 
      "Build pair should exist" 
      expected maybeBuildPair
  )  
  
testUpdateFastExecution :: Test
testUpdateFastExecution = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    atomically buildStore $ do
      maybeState <- findFastState buildStore (BuildId "build1")
      return ()
    return ()
  )

removeDbDir :: (FilePath, a) -> IO ()
removeDbDir (dbDir, _) = do
  exists <- doesFileExist dbDir
  when exists $ removeDirectoryRecursive dbDir

storeWithBuild :: (FilePath -> IO (BuildStore tx)) -> IO (FilePath, BuildStore tx)
storeWithBuild makeBuildStore = do
  dbDir <- getUniqueDirName
  buildStore <- makeBuildStore dbDir
  atomically buildStore $ do
    createBuildUnlessExists buildStore (BuildId "build1") (VersionId "version1")
  return (dbDir, buildStore)