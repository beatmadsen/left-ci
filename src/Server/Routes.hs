{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
    ( makeApplication
    ) where

import Web.Scotty (ScottyM, get, json, pathParam)
import Control.Monad.IO.Class (liftIO)
import Server.BuildService (BuildService(..))

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
    get "/build/:id" $ do
        buildId <- pathParam "id"
        status <- liftIO $ getBuildStatus service buildId
        json status