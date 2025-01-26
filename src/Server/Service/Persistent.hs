module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service (BuildService (..))
import Server.DataStore (BuildStore (..))
import Server.Domain
    ( BuildSummary(..), BuildState(Running, Init), BuildId(..) )


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
  records <- getBuildRecords buildStore buildId
  if length records == 2
    then return $ Just $ BuildSummary {slowState = Init, fastState = Running}
    else return Nothing
  
