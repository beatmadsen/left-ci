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
import Server.Domain (Build (..), BuildState (..), BuildSummary (..), Version (..), Project (..))
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

pathVersion :: ActionM Version
pathVersion = do
  vid <- pathParam "v"
  return $ Version vid

pathBuild :: ActionM Build
pathBuild = do
  bid <- pathParam "b"
  return $ Build bid

pathProject :: ActionM Project
pathProject = do
  pid <- pathParam "p"
  return $ Project pid

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
  get "/builds/:b" $ do
    bid <- pathBuild
    s <- liftIO $ getBuildSummary service bid
    respondToBuildSummary s

  -- create a build
  post "/projects/:p/versions/:v/builds/:b" $ do
    pid <- pathProject
    vid <- pathVersion
    bid <- pathBuild
    outcome <- liftIO $ createBuild service pid vid bid
    respondToCreationOutcome outcome

  post "/builds/:b/fast/advance" $ do
    bid <- pathBuild
    outcome <- liftIO $ advanceFastSuite service bid
    respondToStateChange outcome

  post "/builds/:b/slow/advance" $ do
    bid <- pathBuild
    outcome <- liftIO $ advanceSlowSuite service bid
    respondToStateChange outcome

  post "/builds/:b/fast/fail" $ do
    bid <- pathBuild
    outcome <- liftIO $ failFastSuite service bid
    respondToStateChange outcome

  post "/builds/:b/slow/fail" $ do
    bid <- pathBuild
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
