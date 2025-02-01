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
      TestLabel "Given a build id in URL, when posting advance slow, then passes build id to service" (testUpdateBuild "slow" "advance"),
      TestLabel "Given a build id in URL, when posting fail fast, then passes build id to service" (testUpdateBuild "fast" "fail"),
      TestLabel "Given a build id in URL, when posting fail slow, then passes build id to service" (testUpdateBuild "slow" "fail"),
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
          { advanceFastResult = x,
            advanceSlowResult = x,
            failFastResult = x,
            failSlowResult = x
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
  passedVersionId <- IORef.newIORef ""
  passedBuildId <- IORef.newIORef ""

  let service =
        defaultService
          { createBuild = \versionId buildId -> do
              IORef.writeIORef passedVersionId versionId
              IORef.writeIORef passedBuildId buildId
              pure SuccessfullyCreated
          }

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- srequest $ makeCreateRequest (VersionId "version-123") (BuildId "build-42")
        assertStatus 200 response
    )
    app

  actualVersionId <- IORef.readIORef passedVersionId
  assertEqual "version id" "version-123" actualVersionId

  actualBuildId <- IORef.readIORef passedBuildId
  assertEqual "build id" "build-42" actualBuildId

testCreateBuildConflict :: Test
testCreateBuildConflict = TestCase $ do
  let service = defaultService {createBuild = (const . const) $ pure Conflict}
  app <- scottyApp $ makeApplication service
  runSession
    ( do
        response <- srequest $ makeCreateRequest (VersionId "version-123") (BuildId "build-42")
        assertStatus 409 response
    )
    app

defaultService :: BuildService
defaultService =
  BuildService
    { getBuildSummary = undefined,
      createBuild = undefined,
      advanceFastResult = undefined,
      advanceSlowResult = undefined,
      failFastResult = undefined,
      failSlowResult = undefined
    }

makeCreateRequest :: VersionId -> BuildId -> SRequest
makeCreateRequest (VersionId vid) (BuildId bid) =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["version", vid, "build", bid]
      }
    "" -- No body needed for create

makeActionRequest :: BuildId -> Text -> Text -> SRequest
makeActionRequest (BuildId bid) cadence action =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["build", bid, cadence, action]
      }
    "" -- No body needed for advance
