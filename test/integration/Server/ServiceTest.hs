{-# LANGUAGE OverloadedStrings #-}

module Server.ServiceTest
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IORef as IORef
import Data.Text (pack)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request, pathInfo, requestBody, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Test (SRequest (..), Session, assertBody, assertStatus, defaultRequest, request, runSession, setRawPathInfo, srequest)
import Server.BuildService
import Server.Domain
import Server.Routes
import Test.HUnit
import Web.Scotty (scottyApp)

testGetStatus :: Test
testGetStatus = TestCase $ do
  service <- makeStubService
  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- request $ defaultRequest {pathInfo = ["build", "123"]}
        assertStatus 200 response
        assertBody "{\"fast\":null,\"slow\":null}" response
    )
    app

testUsesPathBuildId :: Test
testUsesPathBuildId = TestCase $ do
  passedId <- IORef.newIORef ""

  let service =
        BuildService
          { getBuildStatus = \id -> do
              IORef.writeIORef passedId id
              pure $ BuildStatus Nothing Nothing,
            setFastResult = undefined
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

testPostFastResult :: Test
testPostFastResult = TestCase $ do
  passedId <- IORef.newIORef ""
  passedResult <- IORef.newIORef Nothing

  let service =
        BuildService
          { getBuildStatus = undefined,
            setFastResult = \id result -> do
              IORef.writeIORef passedId id
              IORef.writeIORef passedResult (Just result)
              pure ()
          }

  app <- scottyApp $ makeApplication service

  runSession
    ( do
        response <- srequest $ makePostRequest "test-build-42" "{\"result\": \"success\"}"
        assertStatus 200 response
    )
    app

  actualId <- IORef.readIORef passedId
  actualResult <- IORef.readIORef passedResult
  assertEqual "Build ID" "test-build-42" actualId
  assertEqual "Result" (Just SuccessResult) actualResult

makePostRequest :: String -> String -> SRequest
makePostRequest buildId jsonBody =
  SRequest
    defaultRequest
      { requestMethod = "POST",
        pathInfo = ["build", pack buildId, "fast"],
        requestHeaders = [(hContentType, "application/json")]
      }
    (LBS.pack jsonBody)

makeStubService :: IO BuildService
makeStubService =
  pure
    BuildService
      { getBuildStatus = \_ -> pure $ BuildStatus Nothing Nothing,
        setFastResult = \_ _ -> pure ()
      }

tests :: Test
tests =
  TestList
    [ TestLabel "Given a build ID, when getting status, then returns empty status" testGetStatus,
      TestLabel "Given a build ID in URL, when getting status, then passes ID to service" testUsesPathBuildId,
      TestLabel "Given a build ID, when posting success result, then updates fast result" testPostFastResult
    ]