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
      TestLabel "Given build and version ids, when posting advance fast, then passes those ids to service" (testUpdateBuild "fast" "advance"),
      TestLabel "Given build and version ids, when posting advance slow, then passes those ids to service" (testUpdateBuild "slow" "advance"),
      TestLabel "Given build and version ids, when posting fail fast, then passes those ids to service" (testUpdateBuild "fast" "fail"),
      TestLabel "Given build and version ids, when posting fail slow, then passes those ids to service" (testUpdateBuild "slow" "fail")
    ]

testGetSummary :: Test
testGetSummary = TestCase $ do
  let service =
        BuildService
          { getBuildSummary = \_ -> pure $ Just $ BuildSummary Init Init,
            advanceFastResult = undefined,
            advanceSlowResult = undefined,
            failFastResult = undefined,
            failSlowResult = undefined
          }

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
  let service =
        BuildService
          { getBuildSummary = \_ -> pure Nothing,
            advanceFastResult = undefined,
            advanceSlowResult = undefined,
            failFastResult = undefined,
            failSlowResult = undefined
          }
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
        BuildService
          { getBuildSummary = \id -> do
              IORef.writeIORef passedId id
              pure $ Just $ BuildSummary Init Init,
            advanceFastResult = undefined,
            advanceSlowResult = undefined,
            failFastResult = undefined,
            failSlowResult = undefined
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

makePostRequest :: VersionId -> BuildId -> Text -> Text -> SRequest
makePostRequest versionId buildId cadence action =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["version", vid, "build", bid, cadence, action],
        requestHeaders = [(hContentType, "application/json")]
      }
    "" -- No body needed for advance
  where
    (VersionId vid) = versionId
    (BuildId bid) = buildId

testUpdateBuild :: Text -> Text -> Test
testUpdateBuild cadence action = TestCase $ do
  passedBuildId <- IORef.newIORef ""
  passedVersionId <- IORef.newIORef ""

  let x = \versionId buildId -> do
        IORef.writeIORef passedBuildId buildId
        IORef.writeIORef passedVersionId versionId

  let service =
        BuildService
          { getBuildSummary = undefined,
            advanceFastResult = x,
            advanceSlowResult = x,
            failFastResult = x,
            failSlowResult = x
          }

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- srequest $ makePostRequest "version-123" "build-42" cadence action
        assertStatus 200 response
    )
    app

  actualBuildId <- IORef.readIORef passedBuildId
  assertEqual "build id" "build-42" actualBuildId

  actualVersionId <- IORef.readIORef passedVersionId
  assertEqual "version id" "version-123" actualVersionId
