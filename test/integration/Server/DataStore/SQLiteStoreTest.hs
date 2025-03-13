{-# LANGUAGE OverloadedStrings #-}

module Server.DataStore.SQLiteStoreTest
  ( tests,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.SQLite.Simple (close, execute_)
import RandomHelper (getUniqueDirName)
import qualified Server.DataStore as DS
import Server.DataStore.Atomic (AtomicM (..))
import Server.DataStore.SQLiteStore (makeSQLiteBuildStore)
import Server.DataStore.SQLiteStore.CreateBuild (createEntitiesAt)
import Server.DataStore.SQLiteStore.Types (OngoingTransaction (..))
import qualified Server.Domain as D
import System.Directory (doesFileExist, removeDirectoryRecursive)
import Test.HUnit
import Text.Printf (printf)

tests :: Test
tests =
  TestList
    [ TestLabel "Given a created build, when querying, then we can find complete build records for fast and slow suites" testBuildCreation,
      TestLabel "Given a created build, after finding and updating a fast execution of that build, then we can see it updated" testUpdateFastExecution,
      TestLabel "Given a created build, after finding and updating a slow execution of that build, then we can see it updated" testUpdateSlowExecution,
      TestLabel "Given a created build, when listing builds for a project, then we can see the build" testListProjectBuilds,
      TestLabel "Given two builds, when specifying an after time, then we can see the build after that time" testListProjectBuildsAfter,
      TestLabel "Given many builds, when listing builds, then we can only see the latest 10 execution pairs" testListProjectBuildsLimit,
      TestLabel "Given a created build, when finding its project, then we can see the project" testFindProject
    ]

testListProjectBuildsAfter :: Test
testListProjectBuildsAfter =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let project = D.Project "project1"
          let version = D.Version "version1"
          let recentBuild = D.Build "recent"
          pairs <- DS.atomically buildStore $ do
            OngoingTransaction conn <- ask

            let theFirstDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
            liftIO $ createEntitiesAt conn theFirstDate project version (D.Build "b2024")
            let theSecondDate = read "2025-01-01 00:00:00 UTC" :: UTCTime
            liftIO $ createEntitiesAt conn theSecondDate project version (D.Build "b2025")

            DS.findBuildPairs buildStore project (Just theFirstDate)

          -- since store is already set up with a build pair, we should find two, but not three
          assertEqual "Should find two build pairs" 2 (length pairs)
      )

testListProjectBuildsLimit :: Test
testListProjectBuildsLimit =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let project = D.Project "project1"
          let version = D.Version "version1"
          let build = D.Build "build1"

          -- list the builds, and expect 10.
          pairs <- DS.atomically buildStore $ do
            OngoingTransaction conn <- ask

            -- create 15 minutes and for each create a build
            forM_ [1 .. 15] $ \minute -> do
              let build = D.Build $ T.pack ("build-many-" ++ show minute)
              let formattedMinute = printf "%02d" (minute :: Int)
              let theDate = read $ "2024-01-01 00:" ++ formattedMinute ++ ":00 UTC" :: UTCTime

              liftIO $ createEntitiesAt conn theDate project version build
            DS.findBuildPairs buildStore project Nothing
          assertEqual "Should find 10 build pairs" 10 (length pairs)
      )

testBuildCreation :: Test
testBuildCreation =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let build = D.Build "build1"
          let version = D.Version "version1"
          maybeBuildPair <- DS.atomically buildStore $ DS.findBuildPair buildStore build

          case maybeBuildPair of
            Nothing -> assertFailure "Expected build pair but got Nothing"
            Just (DS.BuildPair actual1 actual2) -> do
              assertEqual "First build should match" build (DS.build actual1)
              assertEqual "First version should match" version (DS.version actual1)
              assertEqual "First state should match" D.Init (DS.state actual1)

              assertEqual "Second build should match" build (DS.build actual2)
              assertEqual "Second version should match" version (DS.version actual2)
              assertEqual "Second state should match" D.Init (DS.state actual2)
      )

testUpdateFastExecution :: Test
testUpdateFastExecution =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let build = D.Build "build1"

          foundAndUpdatedE <- ioFindAndUpdateFastState buildStore build

          foundAgainE <- case foundAndUpdatedE of
            Left _ -> return $ Left ()
            Right _ -> ioFindFastState buildStore build

          case foundAgainE of
            Left _ -> assertFailure "Should find again"
            Right state -> assertEqual "Should be running" D.Running state
      )

testUpdateSlowExecution :: Test
testUpdateSlowExecution =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let build = D.Build "build1"

          foundAndUpdatedE <- ioFindAndUpdateSlowState buildStore build

          foundAgainE <- case foundAndUpdatedE of
            Left _ -> return $ Left ()
            Right _ -> ioFindSlowState buildStore build

          case foundAgainE of
            Left _ -> assertFailure "Should find again"
            Right state -> assertEqual "Should be running" D.Running state
      )

testListProjectBuilds :: Test
testListProjectBuilds =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let project = D.Project "project1"
          let version = D.Version "version1"
          let (build1, build2) = (D.Build "build1", D.Build "build2")
          pairs <- DS.atomically buildStore $ do
            DS.createBuildUnlessExists buildStore project version build1
            DS.createBuildUnlessExists buildStore project version build2
            DS.findBuildPairs buildStore project Nothing
          assertEqual "Should find two build pairs" 2 (length pairs)

          let compareBuildRecord expected actual = do
                assertEqual "Build ID should match" (DS.build expected) (DS.build actual)
                assertEqual "Version ID should match" (DS.version expected) (DS.version actual)
                assertEqual "State should match" (DS.state expected) (DS.state actual)

          -- Compare first build pair
          compareBuildRecord (defaultBuildRecord build1 version) (DS.slowSuite $ head pairs)
          compareBuildRecord (defaultBuildRecord build1 version) (DS.fastSuite $ head pairs)

          -- Compare second build pair
          compareBuildRecord (defaultBuildRecord build2 version) (DS.slowSuite $ pairs !! 1)
          compareBuildRecord (defaultBuildRecord build2 version) (DS.fastSuite $ pairs !! 1)
      )

testFindProject :: Test
testFindProject =
  TestCase $
    bracket
      -- setup
      (storeWithBuild makeSQLiteBuildStore)
      -- teardown
      removeDbDir
      -- test
      ( \(dbDir, buildStore) -> do
          let project = D.Project "project1"
          let expected = Just project

          foundProject <- DS.atomically buildStore $ DS.findProject buildStore project

          assertEqual "Should find project" expected foundProject
      )

ioFindFastState :: DS.BuildStore tx -> D.Build -> IO (Either () D.BuildState)
ioFindFastState buildStore build = do
  DS.atomically buildStore $ justToRight <$> DS.findFastState buildStore build

ioFindSlowState :: DS.BuildStore tx -> D.Build -> IO (Either () D.BuildState)
ioFindSlowState buildStore build = do
  DS.atomically buildStore $ justToRight <$> DS.findSlowState buildStore build

ioFindAndUpdateFastState :: DS.BuildStore tx -> D.Build -> IO (Either () ())
ioFindAndUpdateFastState buildStore build = do
  DS.atomically buildStore $ do
    stateE <- justToRight <$> DS.findFastState buildStore build
    case stateE of
      Right _ -> Right <$> DS.updateFastState buildStore build D.Running
      Left _ -> return $ Left ()

ioFindAndUpdateSlowState :: DS.BuildStore tx -> D.Build -> IO (Either () ())
ioFindAndUpdateSlowState buildStore build = do
  DS.atomically buildStore $ do
    stateE <- justToRight <$> DS.findSlowState buildStore build
    case stateE of
      Right _ -> Right <$> DS.updateSlowState buildStore build D.Running
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

defaultBuildRecord :: D.Build -> D.Version -> DS.BuildRecord
defaultBuildRecord build version =
  DS.BuildRecord build version D.Init theDate theDate
  where
    theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime