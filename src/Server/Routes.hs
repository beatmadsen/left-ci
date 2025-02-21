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
import Network.HTTP.Types.Status (status404, status409, status500)
import Server.Domain (Build (..), BuildState (..), BuildSummary (..), Version (..))
import Server.Service (BuildService (..), CreationOutcome (..), StateChangeOutcome (..))
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
    status,
  )
import Web.Scotty.Internal.Types (ActionError, ActionT)

pathVersionId :: ActionM Version
pathVersionId = do
  vid <- pathParam "v"
  return $ Version vid

pathBuildId :: ActionM Build
pathBuildId = do
  bid <- pathParam "b"
  return $ Build bid

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
  get "/build/:b" $ do
    bid <- pathBuildId
    s <- liftIO $ getBuildSummary service bid
    respondToBuildSummary s

  post "/version/:v/build/:b" $ do
    vid <- pathVersionId
    bid <- pathBuildId
    outcome <- liftIO $ createBuild service vid bid
    respondToCreationOutcome outcome

  post "/build/:b/fast/advance" $ do
    bid <- pathBuildId
    outcome <- liftIO $ advanceFastSuite service bid
    respondToStateChange outcome

  post "/build/:b/slow/advance" $ do
    bid <- pathBuildId
    outcome <- liftIO $ advanceSlowSuite service bid
    respondToStateChange outcome

  post "/build/:b/fast/fail" $ do
    bid <- pathBuildId
    outcome <- liftIO $ failFastSuite service bid
    respondToStateChange outcome

  post "/build/:b/slow/fail" $ do
    bid <- pathBuildId
    outcome <- liftIO $ failSlowSuite service bid
    respondToStateChange outcome

respondToStateChange :: StateChangeOutcome -> ActionM ()
respondToStateChange outcome = case outcome of
  NotFound -> status status404
  SuccessfullyChangedState -> json ()

respondToCreationOutcome :: CreationOutcome -> ActionM ()
respondToCreationOutcome outcome = case outcome of
  Conflict -> status status409
  SuccessfullyCreated -> json ()

respondToBuildSummary :: Maybe BuildSummary -> ActionM ()
respondToBuildSummary summary = case summary of
  Nothing -> status status404
  Just summary -> json summary
