{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
  ( makeApplication,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Network.HTTP.Types.Status (status500)
import Server.BuildService (BuildId, BuildService (..))
import Server.Domain (BuildState (..))
import Web.Scotty
  ( ActionM,
    ScottyM,
    catch,
    get,
    json,
    jsonData,
    pathParam,
    post,
    status,
  )
import Web.Scotty.Internal.Types (ActionError)

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
  get "/build/:id" $ do
    buildId <- pathParam "id"
    summary <- liftIO $ getBuildSummary service buildId
    json summary

  post "/version/:v/build/:b/fast/advance" $ do
    versionId <- pathParam "v"
    buildId <- pathParam "b"
    liftIO $ advanceFastResult service versionId buildId
    json ()