module Server.Service.Persistent
  ( makePersistentService,
  )
where

import Server.DataStore (BuildPair (..), BuildRecord (..), BuildStore (..))
import Server.DataStore.Atomic (AtomicM)
import Server.Domain
  ( BuildId (..),
    BuildState (..),
    BuildSummary (..),
    VersionId (..),
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

pGetBuildSummary :: BuildStore ctx -> BuildId -> IO (Maybe BuildSummary)
pGetBuildSummary buildStore buildId =
  atomically buildStore $ do
    maybeBuildPair <- findBuildPair buildStore buildId
    pure $ fmap extractSummary maybeBuildPair

extractSummary :: BuildPair -> BuildSummary
extractSummary bp = BuildSummary {slowState = state (slowBuild bp), fastState = state (fastBuild bp)}

pCreateBuild :: BuildStore ctx -> VersionId -> BuildId -> IO CreationOutcome
pCreateBuild buildStore versionId buildId =
  atomically buildStore $ do
    result <- createBuildUnlessExists buildStore buildId versionId
    pure $ case result of
      Left () -> Conflict
      Right () -> SuccessfullyCreated

pAdvanceFastSuite :: BuildStore ctx -> BuildId -> IO StateChangeOutcome
pAdvanceFastSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (updateFastState buildStore buildId) state

pAdvanceSlowSuite :: BuildStore ctx -> BuildId -> IO StateChangeOutcome
pAdvanceSlowSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findSlowState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (updateSlowState buildStore buildId) state

pFailFastSuite :: BuildStore ctx -> BuildId -> IO StateChangeOutcome
pFailFastSuite buildStore buildId = atomically buildStore $ do
  maybeState <- findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just Failed -> pure SuccessfullyChangedState
    Just _ -> do
      updateFastState buildStore buildId Failed
      pure SuccessfullyChangedState

pFailSlowSuite :: BuildStore ctx -> BuildId -> IO StateChangeOutcome
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
