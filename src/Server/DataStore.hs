module Server.DataStore
  ( BuildStore (..),
    BuildRecord (..)
  )
where

import Server.Domain

data BuildRecord = BuildRecord
  { buildId :: BuildId
  , versionId :: VersionId
  , cadence :: Cadence
  , state :: BuildState
  }

data BuildStore = BuildStore 
  { getBuildRecords :: BuildId -> IO [BuildRecord]  -- Takes a build ID, returns matching rows
  }
