{-# LANGUAGE OverloadedStrings #-}

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
import Network.Wai (Application, Request, pathInfo, requestBody, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Test (SRequest (..), Session, assertBody, assertStatus, defaultRequest, request, runSession, setRawPathInfo, srequest)
import Server.Domain
import Server.Routes
import Server.Service
import Test.HUnit
import Web.Scotty (scottyApp)

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
      TestLabel "Given a service that reports conflict, when creating build, then returns status code 409" testCreateBuildConflict
    ]

testGetSummary :: Test
testGetSummary = TestCase $ do
  let service = defaultService {getBuildSummary = \_ -> pure $ Just $ BuildSummary Init Init}

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["build", "123"]}
        assertStatus 200 response
        assertBody "{\"fast\":\"init\",\"slow\":\"init\"}" response
    )
    app

testGetSummaryNotFound :: Test
testGetSummaryNotFound = TestCase $ do
  let service = defaultService {getBuildSummary = \_ -> pure Nothing}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["build", "123"]}
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
              pure $ Just $ BuildSummary Init Init
          }

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <-
          request $
            defaultRequest
              { pathInfo = ["build", "test-build-42"]
              }
        assertStatus 200 response
    )
    app

  actualId <- IORef.readIORef passedId
  assertEqual "Build ID" "test-build-42" actualId

testUpdateBuild :: Text -> Text -> Test
testUpdateBuild cadence action = TestCase $ do
  passedBuildId <- IORef.newIORef ""

  let x = \buildId -> do
        IORef.writeIORef passedBuildId buildId
        pure SuccessfullyChangedState

  let service =
        defaultService
          { advanceFastSuite = x,
            advanceSlowSuite = x,
            failFastSuite = x,
            failSlowSuite = x
          }

  app <- scottyApp $ makeApplication service

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

  app <- scottyApp $ makeApplication service

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
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeCreateRequest (Project "project-123") (Version "version-123") (Build "build-42")
        assertStatus 409 response
    )
    app

testAdvanceFastResultNonExistent :: Test
testAdvanceFastResultNonExistent = TestCase $ do
  let service = defaultService {advanceFastSuite = const $ pure NotFound}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "fast" "advance"
        assertStatus 404 response
    )
    app

testAdvanceSlowResultNonExistent :: Test
testAdvanceSlowResultNonExistent = TestCase $ do
  let service = defaultService {advanceSlowSuite = const $ pure NotFound}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "slow" "advance"
        assertStatus 404 response
    )
    app

testFailFastResultNonExistent :: Test
testFailFastResultNonExistent = TestCase $ do
  let service = defaultService {failFastSuite = const $ pure NotFound}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "fast" "fail"
        assertStatus 404 response
    )
    app

testFailSlowResultNonExistent :: Test
testFailSlowResultNonExistent = TestCase $ do
  let service = defaultService {failSlowSuite = const $ pure NotFound}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeActionRequest (Build "build-42") "slow" "fail"
        assertStatus 404 response
    )
    app

defaultService :: BuildService
defaultService =
  BuildService
    { getBuildSummary = undefined,
      createBuild = undefined,
      advanceFastSuite = undefined,
      advanceSlowSuite = undefined,
      failFastSuite = undefined,
      failSlowSuite = undefined
    }

makeCreateRequest :: Project -> Version -> Build -> SRequest
makeCreateRequest (Project name) (Version vid) (Build bid) =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["project", name, "version", vid, "build", bid]
      }
    "" -- No body needed for create

makeActionRequest :: Build -> Text -> Text -> SRequest
makeActionRequest (Build bid) cadence action =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["build", bid, cadence, action]
      }
    "" -- No body needed for advance
