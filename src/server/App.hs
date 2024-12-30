{-# LANGUAGE OverloadedStrings #-}
module Server.App
    ( app
    , runWithArgsAndServer
    ) where

import Web.Scotty ( get, scottyApp, ScottyM )
import Network.Wai ( Application )
import Server.Routes ( helloHandler, goodbyeHandler, echoHandler )

app :: ScottyM ()
app = do
    get "/hello" helloHandler
    get "/goodbye" goodbyeHandler
    get "/echo" echoHandler

runWithArgsAndServer :: [String] -> (Application -> IO ()) -> IO ()
runWithArgsAndServer args server = scottyApp app >>= server
