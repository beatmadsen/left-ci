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
import Network.HTTP.Types.Status (status500, status404, status409)
import Server.Service (BuildService (..), CreationOutcome (..), StateChangeOutcome (..))
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
    s <- liftIO $ getBuildSummary service bid    
    case s of
      Nothing -> status status404
      Just summary -> json summary

  post "/version/:v/build/:b" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    outcome <- liftIO $ createBuild service vid bid
    case outcome of
      Conflict -> status status409
      SuccessfullyCreated -> json ()

  post "/build/:b/fast/advance" $ do
    bid <- pathBuildId
    outcome <- liftIO $ advanceFastResult service bid
    case outcome of
      NotFound -> status status404
      SuccessfullyChangedState -> json ()

  post "/build/:b/slow/advance" $ do
    bid <- pathBuildId
    outcome <- liftIO $ advanceSlowResult service bid
    case outcome of
      NotFound -> status status404
      SuccessfullyChangedState -> json ()

  post "/build/:b/fast/fail" $ do
    bid <- pathBuildId
    liftIO $ failFastResult service bid
    json ()

  post "/build/:b/slow/fail" $ do
    bid <- pathBuildId
    liftIO $ failSlowResult service bid
    json ()