module Server.Service.Persistent
  ( makePersistentService
  )
where

import Server.Service (BuildService (..))
import Server.DataStore (BuildStore (..))

makePersistentService :: BuildStore -> BuildService
makePersistentService buildStore = BuildService { 
    getBuildSummary = const $ pure Nothing,
    advanceFastResult = undefined,
    advanceSlowResult = undefined,
    failFastResult = undefined,
    failSlowResult = undefined
  }