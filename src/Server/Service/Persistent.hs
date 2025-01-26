module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service (BuildService (..))
import Server.DataStore (BuildStore (..), BuildRecord (..), BuildPair (..))
import Server.Domain
    ( BuildSummary(..), BuildState(..), BuildId(..), VersionId(..) )


makePersistentService :: BuildStore -> BuildService
makePersistentService buildStore = BuildService { 
    getBuildSummary = pGetBuildSummary buildStore,
    createBuild = undefined,
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

pAdvanceFastResult :: BuildStore -> BuildId -> IO ()
pAdvanceFastResult buildStore buildId = undefined
