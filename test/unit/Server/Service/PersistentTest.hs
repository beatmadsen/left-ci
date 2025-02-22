{-# LANGUAGE OverloadedStrings #-}

module Server.Service.PersistentTest
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Data.IORef
import qualified Server.DataStore as DS
import Server.DataStore.Atomic (AtomicM (..))
import qualified Server.Domain as D
import Server.Service
import Server.Service.Persistent (makePersistentService)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertBool, (@?=))
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)

tests :: Test
tests =
  TestList
    [ TestLabel "given a store and a non-existent build id, getBuildSummary returns Nothing" testGetBuildSummaryNonExistent,
      TestLabel "given a store that returns two rows for a build id, getBuildSummary returns a summary" testGetBuildSummaryTwoRows,
      TestLabel "given a store that fails to find a project, listProjectBuilds returns Nothing" testListProjectBuildsFails,
      TestLabel "given a store that returns a project, listProjectBuilds returns the list of builds" testListProjectBuilds,
      TestLabel "given a store that reports build id already exists, createBuild reports conflict" testCreateBuildAlreadyExists,
      TestLabel "given a store that reports build id does not exist, createBuild reports success" testCreateBuildSuccess,
      TestLabel "given a store and a non-existent build id, advanceFastSuite reports NotFound" testAdvanceFastResultNonExistent,
      TestLabel "given a store that returns a fast state, and succeeds in updating it, advanceFastSuite reports SuccessfullyChangedState" testAdvanceFastResultSuccess,
      TestLabel "given a store that returns a fast state, advanceFastSuite advances and updates the fast state" testAdvanceFastResultAdvancesAndUpdates,
      TestLabel "given a store that returns a slow state, and succeeds in updating it, advanceSlowSuite reports SuccessfullyChangedState" testAdvanceSlowResultSuccess,
      TestLabel "given a store that returns a slow state, advanceSlowSuite advances and updates the slow state" testAdvanceSlowResultAdvancesAndUpdates,
      TestLabel "given a store that returns a fast state, and succeeds in updating it, failFastSuite reports SuccessfullyChangedState" testFailFastResultSuccess,
      TestLabel "given a store that returns a fast state, failFastSuite advances and updates the fast state" testFailFastResultAdvancesAndUpdates,
      TestLabel "given a store that returns a slow state, and succeeds in updating it, failSlowSuite reports SuccessfullyChangedState" testFailSlowResultSuccess,
      TestLabel "given a store that returns a slow state, failSlowSuite advances and updates the slow state" testFailSlowResultAdvancesAndUpdates
    ]

testGetBuildSummaryNonExistent :: Test
testGetBuildSummaryNonExistent = TestCase $ do
  let service = makePersistentService defaultStore {DS.findBuildPair = const $ pure Nothing}
  actual <- getBuildSummary service (D.Build "123")
  let expected = Nothing
  actual @?= expected

testGetBuildSummaryTwoRows :: Test
testGetBuildSummaryTwoRows = TestCase $ do
  let service =
        makePersistentService
          defaultStore
            { DS.findBuildPair = const $ pure $ Just defaultBuildPair
            }
  actual <- getBuildSummary service (D.Build "123")
  let theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
  let expected = Just $ D.BuildSummary
                   { D.slowSuite = D.SuiteSummary 
                       { D.state = D.Init
                       , D.createdAt = theDate
                       , D.updatedAt = theDate
                       }
                   , D.fastSuite = D.SuiteSummary 
                       { D.state = D.Running
                       , D.createdAt = theDate
                       , D.updatedAt = theDate
                       }
                   }
  actual @?= expected

testCreateBuildAlreadyExists :: Test
testCreateBuildAlreadyExists = TestCase $ do
  let service = makePersistentService defaultStore {DS.createBuildUnlessExists = (const . const . const) $ pure $ Left ()}
  actual <- createBuild service (D.Project "abc") (D.Version "123") (D.Build "456")
  let expected = Conflict
  actual @?= expected

testCreateBuildSuccess :: Test
testCreateBuildSuccess = TestCase $ do
  let service = makePersistentService defaultStore {DS.createBuildUnlessExists = (const . const . const) $ pure $ Right ()}
  actual <- createBuild service (D.Project "abc") (D.Version "123") (D.Build "456")
  let expected = SuccessfullyCreated
  actual @?= expected

testAdvanceFastResultNonExistent :: Test
testAdvanceFastResultNonExistent = TestCase $ do
  let service = makePersistentService defaultStore {DS.findFastState = const $ pure Nothing}
  actual <- advanceFastSuite service (D.Build "123")
  let expected = NotFound
  actual @?= expected

testAdvanceFastResultSuccess :: Test
testAdvanceFastResultSuccess = TestCase $ do
  let service = makeServiceWithFastStubs D.Running
  actual <- advanceFastSuite service (D.Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testAdvanceFastResultAdvancesAndUpdates :: Test
testAdvanceFastResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef D.Init
  let service = makeServiceWithFastMocks writtenState
  advanceFastSuite service (D.Build "123")
  actual <- readIORef writtenState
  let expected = D.Running
  actual @?= expected

testAdvanceSlowResultSuccess :: Test
testAdvanceSlowResultSuccess = TestCase $ do
  let service = makeServiceWithSlowStubs D.Init
  actual <- advanceSlowSuite service (D.Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testAdvanceSlowResultAdvancesAndUpdates :: Test
testAdvanceSlowResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef D.Init
  let service = makeServiceWithSlowMocks writtenState
  advanceSlowSuite service (D.Build "123")
  actual <- readIORef writtenState
  let expected = D.Running
  actual @?= expected

testFailFastResultSuccess :: Test
testFailFastResultSuccess = TestCase $ do
  let service = makeServiceWithFastStubs D.Running
  actual <- failFastSuite service (D.Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testFailFastResultAdvancesAndUpdates :: Test
testFailFastResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef D.Running
  let service = makeServiceWithFastMocks writtenState
  failFastSuite service (D.Build "123")
  actual <- readIORef writtenState
  let expected = D.Failed
  actual @?= expected

testFailSlowResultSuccess :: Test
testFailSlowResultSuccess = TestCase $ do
  let service = makeServiceWithSlowStubs D.Running
  actual <- failSlowSuite service (D.Build "123")
  let expected = SuccessfullyChangedState
  actual @?= expected

testFailSlowResultAdvancesAndUpdates :: Test
testFailSlowResultAdvancesAndUpdates = TestCase $ do
  writtenState <- newIORef D.Running
  let service = makeServiceWithSlowMocks writtenState
  failSlowSuite service (D.Build "123")
  actual <- readIORef writtenState
  let expected = D.Failed
  actual @?= expected

testListProjectBuildsFails :: Test
testListProjectBuildsFails = TestCase $ do
  let service = makePersistentService defaultStore {DS.findProject = const $ pure Nothing}
  actual <- listProjectBuilds service (D.Project "abc")
  let expected = Nothing
  actual @?= expected

testListProjectBuilds :: Test
testListProjectBuilds = TestCase $ do
  let otherPair = makeBuildPair (D.Build "estum1")
  let service = makePersistentService defaultStore {
    DS.findProject = const $ pure $ Just (D.Project "abc"),     
    DS.findBuildPairs = const $ pure [defaultBuildPair, otherPair]
  }
  actual <- listProjectBuilds service (D.Project "abc")
  let expected = Just $ makeBuildMap
  actual @?= expected
  
makeBuildMap :: BuildMap
makeBuildMap = 
  let theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
      summaries = 
        [ ("123", D.BuildSummary 
            { D.slowSuite = D.SuiteSummary {D.state = D.Init, D.createdAt = theDate, D.updatedAt = theDate}, 
              D.fastSuite = D.SuiteSummary {D.state = D.Running, D.createdAt = theDate, D.updatedAt = theDate}
            }
          ), 
          ("estum1", D.BuildSummary 
            { D.slowSuite = D.SuiteSummary {D.state = D.Init, D.createdAt = theDate, D.updatedAt = theDate}, 
              D.fastSuite = D.SuiteSummary {D.state = D.Running, D.createdAt = theDate, D.updatedAt = theDate}
            }
          )]  
  in Map.fromList summaries

makeServiceWithFastStubs :: D.BuildState -> BuildService
makeServiceWithFastStubs inital =
  makePersistentService
    defaultStore
      { DS.findFastState = makeStubbedFind inital,
        DS.updateFastState = makeStubbedUpdate
      }

makeServiceWithSlowStubs :: D.BuildState -> BuildService
makeServiceWithSlowStubs inital =
  makePersistentService
    defaultStore
      { DS.findSlowState = makeStubbedFind inital,
        DS.updateSlowState = makeStubbedUpdate
      }

makeStubbedFind :: D.BuildState -> (D.Build -> AtomicM ctx (Maybe D.BuildState))
makeStubbedFind state = const $ pure $ Just state

makeStubbedUpdate :: (D.Build -> D.BuildState -> AtomicM ctx ())
makeStubbedUpdate = (const . const) $ pure ()

makeMockedFind :: IORef D.BuildState -> (D.Build -> AtomicM ctx (Maybe D.BuildState))
makeMockedFind ref = const $ fmap Just $ liftIO $ readIORef ref

makeMockedUpdate :: IORef D.BuildState -> (D.Build -> D.BuildState -> AtomicM ctx ())
makeMockedUpdate ref _ newState = liftIO $ writeIORef ref newState

makeServiceWithFastMocks :: IORef D.BuildState -> BuildService
makeServiceWithFastMocks ref =
  makePersistentService
    defaultStore
      { DS.findFastState = makeMockedFind ref,
        DS.updateFastState = makeMockedUpdate ref
      }

makeServiceWithSlowMocks :: IORef D.BuildState -> BuildService
makeServiceWithSlowMocks ref =
  makePersistentService
    defaultStore
      { DS.findSlowState = makeMockedFind ref,
        DS.updateSlowState = makeMockedUpdate ref
      }

defaultBuildPair :: DS.BuildPair
defaultBuildPair = makeBuildPair (D.Build "123")

makeBuildPair :: D.Build -> DS.BuildPair
makeBuildPair build = 
  let version = D.Version "04a66b1n"
      theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
  in DS.BuildPair 
    { DS.slowSuite = DS.BuildRecord build version D.Init theDate theDate
    , DS.fastSuite = DS.BuildRecord build version D.Running theDate theDate
    }


defaultAtomically :: AtomicM ctx a -> IO a
defaultAtomically action =
  runReaderT (runAtomicM action) undefined

defaultStore :: DS.BuildStore ctx
defaultStore =
  DS.BuildStore
    { DS.atomically = defaultAtomically,
      DS.findBuildPair = undefined,
      DS.findProject = undefined,
      DS.findBuildPairs = undefined,
      DS.createBuildUnlessExists = undefined,
      DS.findFastState = undefined,
      DS.updateFastState = undefined,
      DS.findSlowState = undefined,
      DS.updateSlowState = undefined
    }
