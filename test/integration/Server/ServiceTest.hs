{-# LANGUAGE OverloadedStrings #-}

module Server.ServiceTest
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Network.HTTP.Types
import Network.Wai (Application, pathInfo)
import Network.Wai.Test (Session, assertBody, assertStatus, defaultRequest, request, runSession)
import Test.HUnit
import Web.Scotty (ScottyM, get, json, scotty, scottyApp)

data TestResult = SuccessResult | FailureResult deriving (Show, Eq)

data BuildStatus = BuildStatus
  { fastResult :: Maybe TestResult,
    slowResult :: Maybe TestResult
  }
  deriving (Show, Eq)

data BuildService = BuildService
  { getBuildStatus :: String -> IO BuildStatus
  }

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
        response <- request $ defaultRequest { pathInfo = ["build", "123"] }
        assertStatus 200 response
        assertBody "{\"fast\":null,\"slow\":null}" response
    )
    app

instance ToJSON TestResult where
  toJSON SuccessResult = "success"
  toJSON FailureResult = "failure"

instance ToJSON BuildStatus where
  toJSON (BuildStatus fast slow) =
    object
      [ "fast" .= fast,
        "slow" .= slow
      ]

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
  get "/build/:id" $ do
    status <- liftIO $ getBuildStatus service "123"
    json status

tests :: Test
tests =
  TestList
    [ TestLabel "Get status endpoint" testGetStatus
    ]