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
import qualified Server.DataStore as DS
import qualified Server.Domain as D

import Data.Time.Clock (UTCTime)

import Server.DataStore.Atomic (AtomicM(..))

import Control.Monad.Except (ExceptT(..), runExceptT)


tests :: Test
tests =
  TestList
    [ TestLabel "Given a created build, when querying, then we can find complete build records for fast and slow suites" testBuildCreation,
      TestLabel "Given a created build, after finding and updating a fast execution of that build, then we can see it updated" testUpdateFastExecution,
      TestLabel "Given a created build, after finding and updating a slow execution of that build, then we can see it updated" testUpdateSlowExecution,
      TestLabel "Given a created build, when listing builds for a project, then we can see the build" testListProjectBuilds,
      TestLabel "Given a created build, when finding its project, then we can see the project" testFindProject
    ]

testBuildCreation :: Test
testBuildCreation = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let build = D.Build "build1"
    let version = D.Version "version1"
    maybeBuildPair <- DS.atomically buildStore $ DS.findBuildPair buildStore build

    let expected = Just $ DS.BuildPair (defaultBuildRecord build version) (defaultBuildRecord build version)
    
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
    let buildId = D.Build "build1"
    
    foundAndUpdatedE <- ioFindAndUpdateFastState buildStore buildId
    
    foundAgainE <- case foundAndUpdatedE of
      Left _ -> return $ Left ()
      Right _ -> ioFindFastState buildStore buildId
    
    case foundAgainE of
      Left _ -> assertFailure "Should find again"
      Right state -> assertEqual "Should be running" D.Running state
  )

testUpdateSlowExecution :: Test
testUpdateSlowExecution = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let buildId = D.Build "build1"
    
    foundAndUpdatedE <- ioFindAndUpdateSlowState buildStore buildId
    
    foundAgainE <- case foundAndUpdatedE of
      Left _ -> return $ Left ()
      Right _ -> ioFindSlowState buildStore buildId
    
    case foundAgainE of
      Left _ -> assertFailure "Should find again"
      Right state -> assertEqual "Should be running" D.Running state
  )

testListProjectBuilds :: Test
testListProjectBuilds = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let project = D.Project "project1"
    let version = D.Version "version1"
    let (build1, build2) = (D.Build "build1", D.Build "build2")
    pairs <- DS.atomically buildStore $ do
      DS.createBuildUnlessExists buildStore project version build1
      DS.createBuildUnlessExists buildStore project version build2
      DS.findBuildPairs buildStore project
    let expected = [DS.BuildPair {
      DS.slowSuite = defaultBuildRecord build1 version,
      DS.fastSuite = defaultBuildRecord build1 version
    }, DS.BuildPair {
      DS.slowSuite = defaultBuildRecord build2 version,
      DS.fastSuite = defaultBuildRecord build2 version
    }]
    assertEqual "Should find two builds" expected pairs
  )

defaultBuildRecord :: D.Build -> D.Version -> DS.BuildRecord
defaultBuildRecord build version = 
  DS.BuildRecord build version D.Init theDate theDate
  where
    theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime

testFindProject :: Test
testFindProject = TestCase $ bracket
  -- setup
  (storeWithBuild makeSQLiteBuildStore)
  -- teardown
  removeDbDir
  -- test
  (\(dbDir, buildStore) -> do
    let project = D.Project "project1"
    let expected = Just project

    foundProject <- DS.atomically buildStore $ DS.findProject buildStore project

    assertEqual "Should find project" expected foundProject
  )

ioFindFastState :: DS.BuildStore tx -> D.Build -> IO (Either () D.BuildState)
ioFindFastState buildStore buildId = do
  DS.atomically buildStore $ justToRight <$> DS.findFastState buildStore buildId

ioFindSlowState :: DS.BuildStore tx -> D.Build -> IO (Either () D.BuildState)
ioFindSlowState buildStore buildId = do
  DS.atomically buildStore $ justToRight <$> DS.findSlowState buildStore buildId
    

ioFindAndUpdateFastState :: DS.BuildStore tx -> D.Build -> IO (Either () ())
ioFindAndUpdateFastState buildStore buildId = do
  DS.atomically buildStore $ do  
    stateE <- justToRight <$> DS.findFastState buildStore buildId
    case stateE of
      Right _ -> Right <$> DS.updateFastState buildStore buildId D.Running
      Left _ -> return $ Left ()

ioFindAndUpdateSlowState :: DS.BuildStore tx -> D.Build -> IO (Either () ())
ioFindAndUpdateSlowState buildStore buildId = do
  DS.atomically buildStore $ do
    stateE <- justToRight <$> DS.findSlowState buildStore buildId
    case stateE of
      Right _ -> Right <$> DS.updateSlowState buildStore buildId D.Running
      Left _ -> return $ Left ()

justToRight :: Maybe a -> Either () a
justToRight (Just a) = Right a
justToRight Nothing = Left ()

removeDbDir :: (FilePath, a) -> IO ()
removeDbDir (dbDir, _) = do
  exists <- doesFileExist dbDir
  when exists $ removeDirectoryRecursive dbDir

storeWithBuild :: (FilePath -> IO (DS.BuildStore tx)) -> IO (FilePath, DS.BuildStore tx)
storeWithBuild makeBuildStore = do
  dbDir <- getUniqueDirName
  buildStore <- makeBuildStore dbDir
  DS.atomically buildStore $ do
    DS.createBuildUnlessExists buildStore (D.Project "project1") (D.Version "version1") (D.Build "build1")
  return (dbDir, buildStore)