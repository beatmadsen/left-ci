{-# LANGUAGE OverloadedStrings #-}
module Server.App
    ( app
    , runWithArgsAndServer
    ) where

import Web.Scotty
import Network.Wai
import Server.Routes

app :: ScottyM ()
app = do
    get "/hello" helloHandler
    get "/goodbye" goodbyeHandler
    get "/echo" echoHandler

runWithArgsAndServer :: [String] -> (Application -> IO ()) -> IO ()
runWithArgsAndServer args server = scottyApp app >>= server
