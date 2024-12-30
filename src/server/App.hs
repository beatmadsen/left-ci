{-# LANGUAGE OverloadedStrings #-}

module Server.App
  ( app,
    runWithArgsAndServer,
    AppRunner,
  )
where

import Network.Wai (Application)
import Server.Routes (echoHandler, goodbyeHandler, helloHandler)
import Web.Scotty (ScottyM, get, scottyApp)

app :: ScottyM ()
app = do
  get "/hello" helloHandler
  get "/goodbye" goodbyeHandler
  get "/echo" echoHandler

type AppRunner = Application -> IO ()

runWithArgsAndServer :: [String] -> AppRunner -> IO ()
runWithArgsAndServer args appRunner = scottyApp app >>= appRunner
