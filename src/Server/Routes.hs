{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
  ( makeApplication,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Either (Either (..))
import Data.Text (Text)
import Network.HTTP.Types.Status (status500)
import Server.BuildService (BuildService (..))
import Server.Domain (BuildId (..), BuildState (..), VersionId (..))
import Web.Scotty
  ( ActionM,
    Parsable,
    ScottyM,
    catch,
    get,
    json,
    jsonData,
    pathParam,
    post,
    status
  )


import Web.Scotty.Internal.Types (ActionError, ActionT)

pathVersionId :: ActionM VersionId
pathVersionId = do
  vid <- pathParam "v"
  return $ VersionId vid

pathBuildId :: ActionM BuildId
pathBuildId = do
  bid <- pathParam "b"
  return $ BuildId bid

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do

  get "/build/:b" $ do    
    bid <- pathBuildId    
    summary <- liftIO $ getBuildSummary service bid    
    json summary

  post "/version/:v/build/:b/fast/advance" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    liftIO $ advanceFastResult service vid bid
    json ()

  post "/version/:v/build/:b/slow/advance" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    liftIO $ advanceSlowResult service vid bid
    json ()

  post "/version/:v/build/:b/fast/fail" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    liftIO $ failFastResult service vid bid
    json ()

  post "/version/:v/build/:b/slow/fail" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    liftIO $ failSlowResult service vid bid
    json ()