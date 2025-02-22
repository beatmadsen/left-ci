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
import Server.Domain (Project(..), Version(..), Build(..), BuildState(..), BuildSummary(..))
import Server.DataStore (BuildPair(..), BuildRecord(..))

import Server.DataStore.Atomic (AtomicM(..))

import Control.Monad.Except (ExceptT(..), runExceptT)


tests :: Test
tests =
  TestList
    [ TestLabel "Given a created build, when querying, then we can find complete build records for fast and slow suites" testBuildCreation,
      TestLabel "Given a created build, after finding and updating a fast execution of that build, then we can see it updated" testUpdateFastExecution,
      TestLabel "Given a created build, after finding and updating a slow execution of that build, then we can see it updated" testUpdateSlowExecution,
      TestLabel "Given a created build, when listing builds for a project, then we can see the build" testListProjectBuilds
    ]

testBuildCreation :: Test
testBuildCreation = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    maybeBuildPair <- atomically buildStore $ findBuildPair buildStore (Build "build1")

    let expected = (Just (BuildPair (BuildRecord (Build "build1") (Version "version1") Init) (BuildRecord (Build "build1") (Version "version1") Init)))
    
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
    let buildId = Build "build1"
    
    foundAndUpdatedE <- ioFindAndUpdateFastState buildStore buildId
    
    foundAgainE <- case foundAndUpdatedE of
      Left _ -> return $ Left ()
      Right _ -> ioFindFastState buildStore buildId
    
    case foundAgainE of
      Left _ -> assertFailure "Should find again"
      Right state -> assertEqual "Should be running" Running state
  )

testUpdateSlowExecution :: Test
testUpdateSlowExecution = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let buildId = Build "build1"
    
    foundAndUpdatedE <- ioFindAndUpdateSlowState buildStore buildId
    
    foundAgainE <- case foundAndUpdatedE of
      Left _ -> return $ Left ()
      Right _ -> ioFindSlowState buildStore buildId
    
    case foundAgainE of
      Left _ -> assertFailure "Should find again"
      Right state -> assertEqual "Should be running" Running state
  )

testListProjectBuilds :: Test
testListProjectBuilds = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let project = Project "project1"
    let version = Version "version1"
    let (build1, build2) = (Build "build1", Build "build2")
    pairs <- atomically buildStore $ do
      createBuildUnlessExists buildStore project version build1
      createBuildUnlessExists buildStore project version build2
      findBuildPairs buildStore project
    let expected = [BuildPair {
      slowSuite = BuildRecord build1 version Init,
      fastSuite = BuildRecord build1 version Init
    }, BuildPair {
      slowSuite = BuildRecord build2 version Init,
      fastSuite = BuildRecord build2 version Init
    }]
    assertEqual "Should find two builds" expected pairs
  )


ioFindFastState :: BuildStore tx -> Build -> IO (Either () BuildState)
ioFindFastState buildStore buildId = do
  atomically buildStore $ justToRight <$> findFastState buildStore buildId

ioFindSlowState :: BuildStore tx -> Build -> IO (Either () BuildState)
ioFindSlowState buildStore buildId = do
  atomically buildStore $ justToRight <$> findSlowState buildStore buildId
    

ioFindAndUpdateFastState :: BuildStore tx -> Build -> IO (Either () ())
ioFindAndUpdateFastState buildStore buildId = do
  atomically buildStore $ do
    stateE <- justToRight <$> findFastState buildStore buildId
    case stateE of
      Right _ -> Right <$> updateFastState buildStore buildId Running
      Left _ -> return $ Left ()

ioFindAndUpdateSlowState :: BuildStore tx -> Build -> IO (Either () ())
ioFindAndUpdateSlowState buildStore buildId = do
  atomically buildStore $ do
    stateE <- justToRight <$> findSlowState buildStore buildId
    case stateE of
      Right _ -> Right <$> updateSlowState buildStore buildId Running
      Left _ -> return $ Left ()

justToRight :: Maybe a -> Either () a
justToRight (Just a) = Right a
justToRight Nothing = Left ()

removeDbDir :: (FilePath, a) -> IO ()
removeDbDir (dbDir, _) = do
  exists <- doesFileExist dbDir
  when exists $ removeDirectoryRecursive dbDir

storeWithBuild :: (FilePath -> IO (BuildStore tx)) -> IO (FilePath, BuildStore tx)
storeWithBuild makeBuildStore = do
  dbDir <- getUniqueDirName
  buildStore <- makeBuildStore dbDir
  atomically buildStore $ do
    createBuildUnlessExists buildStore (Project "project1") (Version "version1") (Build "build1")
  return (dbDir, buildStore)