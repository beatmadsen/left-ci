{-# LANGUAGE OverloadedStrings #-}

module Server.ServiceTest
  ( tests,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, pathInfo, requestBody, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Test (Session, assertBody, assertStatus, defaultRequest, request, runSession)
import Server.BuildService
import Server.Domain
import Server.Routes
import Test.HUnit
import Web.Scotty (scottyApp)
import Control.Monad.IO.Class (liftIO)

makeStubService :: IO BuildService
makeStubService =
  pure
    BuildService
      { getBuildStatus = \_ -> pure $ BuildStatus Nothing Nothing,
        setFastResult = \_ _ -> pure ()
      }

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
  -- Create an IORef to store the ID that was passed to the service
  passedId <- IORef.newIORef ""

  let service =
        BuildService
          { getBuildStatus = \id -> do
              IORef.writeIORef passedId id
              pure $ BuildStatus Nothing Nothing,
            setFastResult = \_ _ -> pure ()
          }

  app <- scottyApp $ makeApplication service

  -- Make request with specific build ID
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

  -- Verify the service was called with correct ID
  actualId <- IORef.readIORef passedId
  assertEqual "Build ID" "test-build-42" actualId

testPostFastResult :: Test
testPostFastResult = TestCase $ do
    passedId <- IORef.newIORef ""
    passedResult <- IORef.newIORef Nothing
    
    let service = BuildService
            { getBuildStatus = \_ -> pure $ BuildStatus Nothing Nothing
            , setFastResult = \id result -> do
                IORef.writeIORef passedId id
                IORef.writeIORef passedResult (Just result)
                pure ()
            }
            
    app <- scottyApp $ makeApplication service
    
    runSession (do
        let jsonBody = "{\"result\": \"success\"}"
        let req = defaultRequest 
                { pathInfo = ["build", "test-build-42", "fast"]
                , requestMethod = "POST"
                , requestHeaders = [(hContentType, "application/json")]
                , requestBody = pure $ LBS.toStrict jsonBody
                }
        response <- request req
        assertStatus 200 response
        ) app
        
    actualId <- IORef.readIORef passedId
    actualResult <- IORef.readIORef passedResult
    assertEqual "Build ID" "test-build-42" actualId
    assertEqual "Result" (Just SuccessResult) actualResult

tests :: Test
tests =
  TestList
    [ TestLabel "Get status endpoint" testGetStatus,
      TestLabel "Uses build ID from path" testUsesPathBuildId,
      TestLabel "Post fast result" testPostFastResult
    ]