{-# LANGUAGE OverloadedStrings #-}
module Server.Routes
    ( helloHandler
    , goodbyeHandler
    , echoHandler
    ) where

import Web.Scotty

helloHandler :: ActionM ()
helloHandler = text "Hello, world!"

goodbyeHandler :: ActionM ()
goodbyeHandler = text "Goodbye, world!"

echoHandler :: ActionM ()
echoHandler = do
    msg <- param "msg"
    text $ "Echo: " <> msg
