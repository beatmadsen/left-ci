{-# LANGUAGE OverloadedStrings #-}

module ServerTest
  ( tests,
  )
where

import Test.HUnit
import Control.Exception (bracket)
import System.Directory (removeDirectoryRecursive, doesFileExist)
import RandomHelper (getUniqueDirName, getEphemeralPort)
import Server (makeWaiApp)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Network.HTTP.Client (newManager, defaultManagerSettings, parseRequest, httpNoBody, HttpException(..), Response, httpLbs, responseBody, responseStatus, Request(..))
import Control.Exception (catch)
import Control.Monad (when)
import Network.HTTP.Types (statusCode, methodPost, Method)

tests :: Test
tests =
  TestList
    [ TestLabel "Given a Scotty application, when we start it, then it should run" testX]

testX :: Test
testX = TestCase $ do
  dbDir <- getUniqueDirName
  app <- makeWaiApp dbDir
  port <- getEphemeralPort

  putStrLn $ "Starting server on port " ++ show port

  -- Start the server in a separate thread
  threadId <- forkIO $ run port app
  
  -- Give the server a moment to start
  threadDelay 100000 -- 100ms
  
  -- Try to connect to verify the server is running
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ "http://localhost:" ++ show port ++ "/projects/p1/versions/abcd1234/builds/abcdef"
  let postRequest = request { method = methodPost }
  (statusOk, result) <- (
    do
      response <- httpLbs postRequest manager
      let status = statusCode $ responseStatus response
      return (status == 200, True)
    ) `catch` errorHandler
  
  -- Clean up
  killThread threadId
  exists <- doesFileExist dbDir
  when exists $ removeDirectoryRecursive dbDir
  
  assertBool "Server should start successfully" result
  assertBool "Status code should be 200" statusOk

errorHandler :: HttpException -> IO (Bool, Bool)
errorHandler _ = return (False, False)
