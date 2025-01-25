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
import Server.BuildService
import Server.Domain
import Server.Routes
import Test.HUnit
import Web.Scotty (scottyApp)

tests :: Test
tests =
  TestList
    [ TestLabel "Given a build id, when getting summary, then serialises a populated summary" testGetSummary,
      TestLabel "Given a build id in URL, when getting summary, then passes build id to service" testUsesPathBuildId,
      TestLabel "Given build and version ids, when posting advance fast, then passes those ids to service" (testAdvanceBuild "fast"),
      TestLabel "Given build and version ids, when posting advance slow, then passes those ids to service" (testAdvanceBuild "slow")
    ]

testGetSummary :: Test
testGetSummary = TestCase $ do
  service <- makeStubService
  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["build", "123"]}
        assertStatus 200 response
        assertBody "{\"fast\":\"init\",\"slow\":\"init\"}" response
    )
    app

testUsesPathBuildId :: Test
testUsesPathBuildId = TestCase $ do
  passedId <- IORef.newIORef ""

  let service =
        BuildService
          { getBuildSummary = \id -> do
              IORef.writeIORef passedId id
              pure $ BuildSummary Init Init,
            advanceFastResult = undefined,
            advanceSlowResult = undefined
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

makePostRequest :: String -> String -> Text -> String -> SRequest
makePostRequest versionId buildId cadence action =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["version", pack versionId, "build", pack buildId, cadence, pack action],
        requestHeaders = [(hContentType, "application/json")]
      }
    "" -- No body needed for advance

makeStubService :: IO BuildService
makeStubService =
  pure
    BuildService
      { getBuildSummary = \_ -> pure $ BuildSummary Init Init,
        advanceFastResult = \_ _ -> pure (),
        advanceSlowResult = \_ _ -> pure ()
      }

testAdvanceBuild :: Text -> Test
testAdvanceBuild cadence = TestCase $ do
  passedBuildId <- IORef.newIORef ""
  passedVersionId <- IORef.newIORef ""

  let x = \versionId buildId -> do
        IORef.writeIORef passedBuildId buildId
        IORef.writeIORef passedVersionId versionId

  let service =
        BuildService
          { getBuildSummary = undefined,
            advanceFastResult = x,
            advanceSlowResult = x
          }

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- srequest $ makePostRequest "version-123" "build-42" cadence "advance"
        assertStatus 200 response
    )
    app

  actualBuildId <- IORef.readIORef passedBuildId
  assertEqual "build id" "build-42" actualBuildId

  actualVersionId <- IORef.readIORef passedVersionId
  assertEqual "version id" "version-123" actualVersionId
