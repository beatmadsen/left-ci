{-# LANGUAGE OverloadedStrings #-}
module Server.Routes
    ( helloHandler
    , goodbyeHandler
    , echoHandler
    ) where

import Web.Scotty ( pathParam, text, ActionM )

helloHandler :: ActionM ()
helloHandler = text "Hello, world!"

goodbyeHandler :: ActionM ()
goodbyeHandler = text "Goodbye, world!"

echoHandler :: ActionM ()
echoHandler = do
    msg <- pathParam "msg"
    text $ "Echo: " <> msg
