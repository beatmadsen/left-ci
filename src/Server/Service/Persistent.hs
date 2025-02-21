module Server.Service.Persistent
  ( makePersistentService,
  )
where

import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM)
import Server.Domain
  ( Build (..),
    BuildState (..),
    BuildSummary (..),
    Version (..),
    Project (..)
  )
import Server.Service

makePersistentService :: BuildStore ctx -> BuildService
makePersistentService buildStore =
  BuildService
    { getBuildSummary = pGetBuildSummary buildStore,
      createBuild = pCreateBuild buildStore,
      advanceFastSuite = pAdvanceFastSuite buildStore,
      advanceSlowSuite = pAdvanceSlowSuite buildStore,
      failFastSuite = pFailFastSuite buildStore,
      failSlowSuite = pFailSlowSuite buildStore
    }

pGetBuildSummary :: BuildStore ctx -> Build -> IO (Maybe BuildSummary)
pGetBuildSummary buildStore buildId = do
  maybeBuildPair <- atomically buildStore $ findBuildPair buildStore buildId
  pure $ fmap extractSummary maybeBuildPair

extractSummary :: BuildPair -> BuildSummary
extractSummary bp = BuildSummary {slowState = state (slowSuite bp), fastState = state (fastSuite bp)}

pCreateBuild :: BuildStore ctx -> Project -> Version -> Build -> IO CreationOutcome
pCreateBuild buildStore project version build = do
  result <- atomically buildStore $ createBuildUnlessExists buildStore project version build
  pure $ case result of
    Left () -> Conflict
    Right () -> SuccessfullyCreated

pAdvanceFastSuite :: BuildStore ctx -> Build -> IO StateChangeOutcome
pAdvanceFastSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (updateFastState buildStore buildId) state

pAdvanceSlowSuite :: BuildStore ctx -> Build -> IO StateChangeOutcome
pAdvanceSlowSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findSlowState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (updateSlowState buildStore buildId) state

pFailFastSuite :: BuildStore ctx -> Build -> IO StateChangeOutcome
pFailFastSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just Failed -> pure SuccessfullyChangedState
    Just _ -> do
      updateFastState buildStore buildId Failed
      pure SuccessfullyChangedState

pFailSlowSuite :: BuildStore ctx -> Build -> IO StateChangeOutcome
pFailSlowSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findSlowState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just Failed -> pure SuccessfullyChangedState
    Just _ -> do
      updateSlowState buildStore buildId Failed
      pure SuccessfullyChangedState

type StateUpdater ctx = BuildState -> AtomicM ctx ()

advanceAndUpdate :: StateUpdater ctx -> BuildState -> AtomicM ctx StateChangeOutcome
advanceAndUpdate update state = do
  (update . advance) state
  pure SuccessfullyChangedState
