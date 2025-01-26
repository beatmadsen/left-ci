module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service
import Server.DataStore (BuildStore (..), BuildRecord (..), BuildPair (..))
import Server.Domain
    ( BuildSummary(..), BuildState(..), BuildId(..), VersionId(..) )


makePersistentService :: BuildStore -> BuildService
makePersistentService buildStore = BuildService { 
    getBuildSummary = pGetBuildSummary buildStore,
    createBuild = pCreateBuild buildStore,
    advanceFastResult = pAdvanceFastResult buildStore,
    advanceSlowResult = undefined,
    failFastResult = undefined,
    failSlowResult = undefined
  }

pGetBuildSummary :: BuildStore -> BuildId -> IO (Maybe BuildSummary)
pGetBuildSummary buildStore buildId = do
  maybeBuildPair <- findBuildPair buildStore buildId
  let maybeSummary = fmap extractSummary maybeBuildPair
  return maybeSummary

extractSummary :: BuildPair -> BuildSummary
extractSummary bp = BuildSummary {slowState = state (slowBuild bp), fastState = state (fastBuild bp)}

pCreateBuild :: BuildStore -> VersionId -> BuildId -> IO Outcome
pCreateBuild buildStore versionId buildId = do
  result <- createBuildUnlessExists buildStore buildId versionId
  return $ case result of
    Left () -> Conflict
    Right () -> Success

pAdvanceFastResult :: BuildStore -> BuildId -> IO ()
pAdvanceFastResult buildStore buildId = undefined
