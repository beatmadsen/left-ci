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

import Server.Types
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

tests :: Test
tests =
  TestList
    [ TestLabel "Get status endpoint" testGetStatus
    ]