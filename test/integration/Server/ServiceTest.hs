{-# LANGUAGE OverloadedStrings #-}

module Server.ServiceTest
  ( tests,
  )
where

import Test.HUnit
import Network.Wai(pathInfo)
import Network.Wai.Test (Session, runSession, request, assertBody, assertStatus, defaultRequest)
import Network.HTTP.Types
import Web.Scotty (scottyApp)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Network.Wai (Application)

import Server.BuildService
import Server.Domain
import Server.Routes


makeStubService :: IO BuildService
makeStubService =
  pure
    BuildService
      { getBuildStatus = \_ -> pure $ BuildStatus Nothing Nothing
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
    
    let service = BuildService
            { getBuildStatus = \id -> do
                IORef.writeIORef passedId id
                pure $ BuildStatus Nothing Nothing
            }
            
    app <- scottyApp $ makeApplication service
    
    -- Make request with specific build ID
    runSession (do
        response <- request $ defaultRequest 
            { pathInfo = ["build", "test-build-42"] }
        assertStatus 200 response
        ) app
        
    -- Verify the service was called with correct ID
    actualId <- IORef.readIORef passedId
    assertEqual "Build ID" "test-build-42" actualId

tests :: Test
tests =
  TestList
    [ TestLabel "Get status endpoint" testGetStatus
    , TestLabel "Uses build ID from path" testUsesPathBuildId
    ]