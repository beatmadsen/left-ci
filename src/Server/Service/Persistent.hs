module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service (BuildService (..))
import Server.DataStore (BuildStore (..), BuildRecord (..), BuildPair (..))
import Server.Domain
    ( BuildSummary(..), BuildState(..), BuildId(..) )


makePersistentService :: BuildStore -> BuildService
makePersistentService buildStore = BuildService { 
    getBuildSummary = pGetBuildSummary buildStore,
    advanceFastResult = undefined,
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
