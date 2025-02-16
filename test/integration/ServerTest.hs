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
import Network.HTTP.Client (newManager, defaultManagerSettings, parseRequest, httpNoBody, HttpException(..))
import Control.Exception (catch)
import Control.Monad (when)

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
  threadDelay 1000000 -- 1 seconds
  
  -- Try to connect to verify the server is running
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ "http://localhost:" ++ show port ++ "/build/dummy"
  result <- (
    do
      _ <- httpNoBody request manager
      return True
    ) `catch` errorHandler
    
  -- Clean up
  killThread threadId
  exists <- doesFileExist dbDir
  when exists $ removeDirectoryRecursive dbDir
  
  assertBool "Server should start successfully" result

errorHandler :: HttpException -> IO Bool
errorHandler _ = return False
