module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service
import Server.DataStore (BuildStore (..), BuildRecord (..), BuildPair (..))
import Server.Domain
    ( BuildSummary(..), BuildState(..), BuildId(..), VersionId(..) )


makePersistentService :: BuildStore ctx -> BuildService
makePersistentService buildStore = BuildService { 
    getBuildSummary = pGetBuildSummary buildStore,
    createBuild = pCreateBuild buildStore,
    advanceFastResult = pAdvanceFastResult buildStore,
    advanceSlowResult = undefined,
    failFastResult = undefined,
    failSlowResult = undefined
  }

pGetBuildSummary :: BuildStore ctx -> BuildId -> IO (Maybe BuildSummary)
pGetBuildSummary buildStore buildId = 
  atomically buildStore $ do
    maybeBuildPair <- findBuildPair buildStore buildId
    pure $ fmap extractSummary maybeBuildPair

extractSummary :: BuildPair -> BuildSummary
extractSummary bp = BuildSummary {slowState = state (slowBuild bp), fastState = state (fastBuild bp)}

pCreateBuild :: BuildStore ctx -> VersionId -> BuildId -> IO Outcome
pCreateBuild buildStore versionId buildId = 
  atomically buildStore $ do
    result <- createBuildUnlessExists buildStore buildId versionId
    pure $ case result of
      Left () -> Conflict
      Right () -> Success

pAdvanceFastResult :: BuildStore ctx -> BuildId -> IO ()
pAdvanceFastResult buildStore buildId = undefined
