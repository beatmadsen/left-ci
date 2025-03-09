{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.ServiceTest
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IORef as IORef
import Data.Text (Text, pack)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request, pathInfo, requestBody, requestHeaders, requestMethod, setRequestBodyChunks, queryString)
import Network.Wai.Test (SRequest (..), Session, assertBody, assertStatus, defaultRequest, request, runSession, setRawPathInfo, srequest)
import Server.Domain
import Server.Routes
import Server.Service
import Test.HUnit
import Web.Scotty (scottyApp)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Text.RawString.QQ (r)
import qualified Paths_left_ci as P
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Query, QueryItem)

tests :: Test
tests =
  TestList
    [ TestLabel "Given a build id, when getting summary, then serialises a populated summary" testGetSummary,
      TestLabel "Given a non-existent build id, when getting summary, then returns status code 404 and empty body" testGetSummaryNotFound,
      TestLabel "Given a build id in URL, when getting summary, then passes build id to service" testUsesPathBuildId,
      TestLabel "Given a build id in URL, when posting advance fast, then passes build id to service" (testUpdateBuild "fast" "advance"),
      TestLabel "Given a non-existing build id, when posting advance fast, then returns status code 404" testAdvanceFastResultNonExistent,
      TestLabel "Given a build id in URL, when posting advance slow, then passes build id to service" (testUpdateBuild "slow" "advance"),
      TestLabel "Given a non-existing build id, when posting advance slow, then returns status code 404" testAdvanceSlowResultNonExistent,
      TestLabel "Given a build id in URL, when posting fail fast, then passes build id to service" (testUpdateBuild "fast" "fail"),
      TestLabel "Given a non-existing build id, when posting fail fast, then returns status code 404" testFailFastResultNonExistent,
      TestLabel "Given a build id in URL, when posting fail slow, then passes build id to service" (testUpdateBuild "slow" "fail"),
      TestLabel "Given a non-existing build id, when posting fail slow, then returns status code 404" testFailSlowResultNonExistent,
      TestLabel "Given version and build ids in URL, when creating build, then passes ids to service" testCreateBuild,
      TestLabel "Given a service that reports conflict, when creating build, then returns status code 409" testCreateBuildConflict,
      TestLabel "Given a non-existing project name, when listing builds, then returns status code 404 and empty body" testListProjectBuildsNotFound,
      TestLabel "Given a project with no builds, when listing builds, then returns empty list" testListProjectBuildsEmpty,
      TestLabel "Given a project with builds, when listing builds, then returns list of builds" testListProjectBuilds,
      TestLabel "Given a project name in URL, when listing builds, then passes project name to service" testUsesPathProjectName,
      TestLabel "Given a project with builds and after param, when listing builds, then returns filtered list" testListProjectBuildsAfter,
      TestLabel "Given a project name and after param in URL, when listing builds, then passes both to service" testUsesPathProjectNameAndAfter
    ]

testGetSummary :: Test
testGetSummary = TestCase $ do
  let service = defaultService {getBuildSummary = \_ -> pure $ Just defaultBuildSummary}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["builds", "123"]}
        assertStatus 200 response
        let expectedJson = [r|{
          "fast_suite": {
            "created_at": "2024-05-01T00:00:00Z",
            "state": "init",
            "updated_at": "2024-05-01T00:00:00Z"
          },
          "slow_suite": { 
            "created_at": "2024-05-01T00:00:00Z",
            "state": "init",
            "updated_at": "2024-05-01T00:00:00Z"
          }
        }|]
        assertBody (LBS.pack $ filter (/= ' ') $ filter (/= '\n') expectedJson) response
    )
    app

testGetSummaryNotFound :: Test
testGetSummaryNotFound = TestCase $ do
  let service = defaultService {getBuildSummary = \_ -> pure Nothing}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["builds", "123"]}
        assertStatus 404 response
        assertBody "" response
    )
    app

testUsesPathBuildId :: Test
testUsesPathBuildId = TestCase $ do
  passedId <- IORef.newIORef ""

  let service =
        defaultService
          { getBuildSummary = \id -> do
              IORef.writeIORef passedId id
              pure $ Just defaultBuildSummary
          }

  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <-
          request $
            defaultRequest
              { pathInfo = ["builds", "test-build-42"]
              }
        assertStatus 200 response
    )
    app

  actualId <- IORef.readIORef passedId
  assertEqual "Build ID" "test-build-42" actualId

testUpdateBuild :: Text -> Text -> Test
testUpdateBuild cadence action = TestCase $ do
  passedBuildId <- IORef.newIORef ""

  let x buildId = do
        IORef.writeIORef passedBuildId buildId
        pure SuccessfullyChangedState

  let service =
        defaultService
          { advanceFastSuite = x,
            advanceSlowSuite = x,
            failFastSuite = x,
            failSlowSuite = x
          }

  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <- srequest $ makeActionRequest "build-42" cadence action
        assertStatus 200 response
    )
    app

  actualBuildId <- IORef.readIORef passedBuildId
  assertEqual "build id" "build-42" actualBuildId

testCreateBuild :: Test
testCreateBuild = TestCase $ do
  passedProject <- IORef.newIORef ""
  passedVersion <- IORef.newIORef ""
  passedBuild <- IORef.newIORef ""

  let service =
        defaultService
          { createBuild = \project version build -> do
              IORef.writeIORef passedProject project
              IORef.writeIORef passedVersion version
              IORef.writeIORef passedBuild build
              pure SuccessfullyCreated
          }

  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <- srequest $ makeCreateRequest (Project "project-123") (Version "version-123") (Build "build-42")
        assertStatus 200 response
    )
    app

  actualProject <- IORef.readIORef passedProject
  assertEqual "project" "project-123" actualProject

  actualVersion <- IORef.readIORef passedVersion
  assertEqual "version" "version-123" actualVersion

  actualBuild <- IORef.readIORef passedBuild
  assertEqual "build" "build-42" actualBuild

testCreateBuildConflict :: Test
testCreateBuildConflict = TestCase $ do
  let service = defaultService {createBuild = (const . const . const) $ pure Conflict}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeCreateRequest (Project "project-123") (Version "version-123") (Build "build-42")
        assertStatus 409 response
    )
    app

testAdvanceFastResultNonExistent :: Test
testAdvanceFastResultNonExistent = TestCase $ do
  let service = defaultService {advanceFastSuite = const $ pure NotFound}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "fast" "advance"
        assertStatus 404 response
    )
    app

testAdvanceSlowResultNonExistent :: Test
testAdvanceSlowResultNonExistent = TestCase $ do
  let service = defaultService {advanceSlowSuite = const $ pure NotFound}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "slow" "advance"
        assertStatus 404 response
    )
    app

testFailFastResultNonExistent :: Test
testFailFastResultNonExistent = TestCase $ do
  let service = defaultService {failFastSuite = const $ pure NotFound}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "fast" "fail"
        assertStatus 404 response
    )
    app

testFailSlowResultNonExistent :: Test
testFailSlowResultNonExistent = TestCase $ do
  let service = defaultService {failSlowSuite = const $ pure NotFound}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "slow" "fail"
        assertStatus 404 response
    )
    app

testListProjectBuildsNotFound :: Test
testListProjectBuildsNotFound = TestCase $ do
  let service = defaultService {listProjectBuilds = const $ const $ pure Nothing}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do  
        response <- srequest $ makeListRequest (Project "project-123")
        assertStatus 404 response
    )
    app

testListProjectBuildsEmpty :: Test
testListProjectBuildsEmpty = TestCase $ do
  let service = defaultService {listProjectBuilds = const $ const $ pure (Just Map.empty)}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeListRequest (Project "project-123")
        assertStatus 200 response
        assertBody "{}" response
    )
    app

testListProjectBuilds :: Test
testListProjectBuilds = TestCase $ do
  let theFirstDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
  let theSecondDate = read "2024-01-02 00:00:00 UTC" :: UTCTime
  let service = defaultService {listProjectBuilds = const $ const $ pure $ Just $ Map.fromList [("123", BuildSummary 
    { slowSuite = SuiteSummary {state = Init, createdAt = theFirstDate, updatedAt = theFirstDate}, 
      fastSuite = SuiteSummary {state = Init, createdAt = theFirstDate, updatedAt = theFirstDate}
    }
  ), ("estum1", BuildSummary 
    { slowSuite = SuiteSummary {state = Running, createdAt = theSecondDate, updatedAt = theSecondDate}, 
      fastSuite = SuiteSummary {state = Running, createdAt = theSecondDate, updatedAt = theSecondDate}
    }
  )]}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeListRequest (Project "project-123")
        assertStatus 200 response
        let expectedJson = [r|{
          "123": {
            "fast_suite": {
              "created_at": "2024-01-01T00:00:00Z",
              "state": "init",
              "updated_at": "2024-01-01T00:00:00Z"
            },
            "slow_suite": {
              "created_at": "2024-01-01T00:00:00Z",
              "state": "init",
              "updated_at": "2024-01-01T00:00:00Z"
            }
          },
          "estum1": {
            "fast_suite": {
              "created_at": "2024-01-02T00:00:00Z",
              "state": "running",
              "updated_at": "2024-01-02T00:00:00Z"
            },
            "slow_suite": {
              "created_at": "2024-01-02T00:00:00Z",
              "state": "running",
              "updated_at": "2024-01-02T00:00:00Z"
            }
          }
        }|]
        assertBody (LBS.pack $ filter (/= ' ') $ filter (/= '\n') expectedJson) response
    )
    app

testUsesPathProjectName :: Test
testUsesPathProjectName = TestCase $ do
  passedProject <- IORef.newIORef ""
  let service = defaultService {listProjectBuilds = \project _ -> do
      IORef.writeIORef passedProject project
      pure (Just Map.empty)} 

  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <- srequest $ makeListRequest (Project "project-123")
        assertStatus 200 response
    )
    app

  actualProject <- IORef.readIORef passedProject
  assertEqual "project" "project-123" actualProject

testListProjectBuildsAfter :: Test
testListProjectBuildsAfter = TestCase $ do
  let theFirstDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
  let theSecondDate = read "2024-01-02 00:00:00 UTC" :: UTCTime
  let service = defaultService {listProjectBuilds = \_ mafter -> pure $ Just $ case mafter of
        Just after -> Map.fromList [("estum1", BuildSummary 
          { slowSuite = SuiteSummary {state = Running, createdAt = theSecondDate, updatedAt = theSecondDate}
          , fastSuite = SuiteSummary {state = Running, createdAt = theSecondDate, updatedAt = theSecondDate}
          })]
        Nothing -> Map.empty}
  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service
  runSession
    ( do
        response <- srequest $ makeListRequestWithAfter (Project "project-123") theFirstDate
        assertStatus 200 response
        let expectedJson = [r|{
          "estum1": {
            "fast_suite": {
              "created_at": "2024-01-02T00:00:00Z",
              "state": "running",
              "updated_at": "2024-01-02T00:00:00Z"
            },
            "slow_suite": {
              "created_at": "2024-01-02T00:00:00Z",
              "state": "running",
              "updated_at": "2024-01-02T00:00:00Z"
            }
          }
        }|]
        assertBody (LBS.pack $ filter (/= ' ') $ filter (/= '\n') expectedJson) response
    )
    app

testUsesPathProjectNameAndAfter :: Test
testUsesPathProjectNameAndAfter = TestCase $ do
  passedProject <- IORef.newIORef ""
  passedAfter <- IORef.newIORef Nothing
  let theDate = read "2024-01-01 00:00:00 UTC" :: UTCTime
  let service = defaultService {listProjectBuilds = \project mafter -> do
      IORef.writeIORef passedProject project
      IORef.writeIORef passedAfter mafter
      pure (Just Map.empty)} 

  dataDir <- P.getDataDir
  app <- scottyApp $ makeApplication dataDir service

  runSession
    ( do
        response <- srequest $ makeListRequestWithAfter (Project "project-123") theDate
        assertStatus 200 response
    )
    app

  actualProject <- IORef.readIORef passedProject
  assertEqual "project" "project-123" actualProject
  
  actualAfter <- IORef.readIORef passedAfter
  assertEqual "after" (Just theDate) actualAfter

makeListRequestWithAfter :: Project -> UTCTime -> SRequest
makeListRequestWithAfter (Project name) after =
  SRequest
    defaultRequest
      { requestMethod = "GET",
        pathInfo = ["projects", name, "builds"],
        queryString = [("after", Just $ encodeUtf8 $ pack $ iso8601Show after)]
      }
    ""

defaultService :: BuildService
defaultService =
  BuildService
    { getBuildSummary = undefined,
      listProjectBuilds = undefined,
      createBuild = undefined,
      advanceFastSuite = undefined,
      advanceSlowSuite = undefined,
      failFastSuite = undefined,
      failSlowSuite = undefined
    }


defaultBuildSummary :: BuildSummary
defaultBuildSummary = 
  let theDate = read "2024-05-01 00:00:00 UTC" :: UTCTime
  in BuildSummary {slowSuite = SuiteSummary {state = Init, createdAt = theDate, updatedAt = theDate}, fastSuite = SuiteSummary {state = Init, createdAt = theDate, updatedAt = theDate}}


makeListRequest :: Project -> SRequest
makeListRequest (Project name) =
  SRequest
    defaultRequest
      { requestMethod = "GET",
        pathInfo = ["projects", name, "builds"]
      }
    "" -- No body needed for list

makeCreateRequest :: Project -> Version -> Build -> SRequest
makeCreateRequest (Project name) (Version vid) (Build bid) =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["projects", name, "versions", vid, "builds", bid]
      }
    "" -- No body needed for create

makeActionRequest :: Build -> Text -> Text -> SRequest
makeActionRequest (Build bid) cadence action =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["builds", bid, cadence, action]
      }
    "" -- No body needed for advance
