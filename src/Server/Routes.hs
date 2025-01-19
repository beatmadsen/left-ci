{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
    ( makeApplication
    ) where

import Web.Scotty (ScottyM, get, json)
import Control.Monad.IO.Class (liftIO)
import Server.BuildService (BuildService(..))

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
    get "/build/:id" $ do
        status <- liftIO $ getBuildStatus service "123"
        json status