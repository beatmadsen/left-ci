module Server.Service.Persistent
  ( makePersistentService,
  )
where

import Server.DataStore.Atomic (AtomicM)
import qualified Server.Domain as D
import qualified Server.DataStore as DS
import Server.Service
import qualified Data.Map as Map

makePersistentService :: DS.BuildStore ctx -> BuildService
makePersistentService buildStore =
  BuildService
    { getBuildSummary = pGetBuildSummary buildStore,
      listProjectBuilds = pListProjectBuilds buildStore,
      createBuild = pCreateBuild buildStore,
      advanceFastSuite = pAdvanceFastSuite buildStore,
      advanceSlowSuite = pAdvanceSlowSuite buildStore,
      failFastSuite = pFailFastSuite buildStore,
      failSlowSuite = pFailSlowSuite buildStore
    }

pGetBuildSummary :: DS.BuildStore ctx -> D.Build -> IO (Maybe D.BuildSummary)
pGetBuildSummary buildStore buildId = do
  maybeBuildPair <- DS.atomically buildStore $ DS.findBuildPair buildStore buildId
  pure $ fmap extractSummary maybeBuildPair

pListProjectBuilds :: DS.BuildStore ctx -> D.Project -> IO (Maybe BuildMap)
pListProjectBuilds buildStore project = do
  maybeProject <- DS.atomically buildStore $ DS.findProject buildStore project
  case maybeProject of
    Nothing -> pure Nothing
    Just _ -> do
      pairs <- DS.atomically buildStore $ DS.findBuildPairs buildStore project
      pure $ Just $ convert $ groupByBuild pairs

groupByBuild :: [DS.BuildPair] -> Map.Map D.Build DS.BuildPair
groupByBuild pairs =
  let annotated = [(buildIdFromPair pair, pair) | pair <- pairs]
  in Map.fromListWith const annotated

buildIdFromPair :: DS.BuildPair -> D.Build
buildIdFromPair pair = DS.buildId (DS.fastSuite pair)

convert :: Map.Map D.Build DS.BuildPair -> BuildMap
convert = Map.map extractSummary

extractSummary :: DS.BuildPair -> D.BuildSummary
extractSummary bp = 
  D.BuildSummary {
    D.slowSuite = D.SuiteSummary {
      D.state = DS.state (DS.slowSuite bp),
      D.createdAt = DS.createdAt (DS.slowSuite bp),
      D.updatedAt = DS.updatedAt (DS.slowSuite bp)
    },
    D.fastSuite = D.SuiteSummary {
      D.state = DS.state (DS.fastSuite bp),
      D.createdAt = DS.createdAt (DS.fastSuite bp),
      D.updatedAt = DS.updatedAt (DS.fastSuite bp)
    }
  }

pCreateBuild :: DS.BuildStore ctx -> D.Project -> D.Version -> D.Build -> IO CreationOutcome
pCreateBuild buildStore project version build = do
  result <- DS.atomically buildStore $ DS.createBuildUnlessExists buildStore project version build
  pure $ case result of
    Left () -> Conflict
    Right () -> SuccessfullyCreated

pAdvanceFastSuite :: DS.BuildStore ctx -> D.Build -> IO StateChangeOutcome
pAdvanceFastSuite buildStore buildId = DS.atomically buildStore $ do
  maybeState <- DS.findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (DS.updateFastState buildStore buildId) state

pAdvanceSlowSuite :: DS.BuildStore ctx -> D.Build -> IO StateChangeOutcome
pAdvanceSlowSuite buildStore buildId = DS.atomically buildStore $ do
  maybeState <- DS.findSlowState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just state -> advanceAndUpdate (DS.updateSlowState buildStore buildId) state

pFailFastSuite :: DS.BuildStore ctx -> D.Build -> IO StateChangeOutcome
pFailFastSuite buildStore buildId = DS.atomically buildStore $ do
  maybeState <- DS.findFastState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just D.Failed -> pure SuccessfullyChangedState
    Just _ -> do
      DS.updateFastState buildStore buildId D.Failed
      pure SuccessfullyChangedState

pFailSlowSuite :: DS.BuildStore ctx -> D.Build -> IO StateChangeOutcome
pFailSlowSuite buildStore buildId = DS.atomically buildStore $ do
  maybeState <- DS.findSlowState buildStore buildId
  case maybeState of
    Nothing -> pure NotFound
    Just D.Failed -> pure SuccessfullyChangedState
    Just _ -> do
      DS.updateSlowState buildStore buildId D.Failed
      pure SuccessfullyChangedState

type StateUpdater ctx = D.BuildState -> AtomicM ctx ()

advanceAndUpdate :: StateUpdater ctx -> D.BuildState -> AtomicM ctx StateChangeOutcome
advanceAndUpdate update state = do
  (update . advance) state
  pure SuccessfullyChangedState
