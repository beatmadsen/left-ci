{-# LANGUAGE OverloadedStrings #-}
module Server.Routes
  ( makeApplication,
  )
where


import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Either (Either (..))
import Data.Text (Text)
import Network.HTTP.Types.Status (status400, status404, status409, status500)
import Server.Domain (Build (..), BuildState (..), BuildSummary (..), Version (..), Project (..))
import Data.Time (UTCTime)
import Server.Service
import Web.Scotty
  ( ActionM,
    Parsable,
    ScottyM,
    catch,
    get,
    json,
    jsonData,
    pathParam,
    queryParamMaybe,
    post,
    status,
    file,
    middleware,
  )
import Web.Scotty.Internal.Types (ActionError, ActionT)
import Network.Wai.Middleware.Static

import qualified Web.Scotty.Trans as ST   

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


makeApplication :: FilePath -> BuildService -> ScottyM ()
makeApplication dataDir service = do
  middleware $ staticPolicy (noDots >-> addBase (dataDir </> "site"))

  get "/" $ do file (dataDir </> "site/static/index.html")


  -- list all builds for a project
  get "/projects/:p/builds" $ do
    pid <- pathProject
    mAfter <- getAfter
    s <- liftIO $ listProjectBuilds service pid mAfter
    respondToBuildSummaries s

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

respondToBuildSummaries :: Maybe BuildMap -> ActionM ()
respondToBuildSummaries ms = case ms of
  Nothing -> status status404
  Just summaries -> json summaries

getAfter :: ActionM (Maybe UTCTime)
getAfter = do
  mStr <- queryParamMaybe "after"
  case mStr of
    Nothing -> pure Nothing
    Just str -> case parseAfter str of
      Left _ -> do
        status status400
        return Nothing
      Right time -> return (Just time)
