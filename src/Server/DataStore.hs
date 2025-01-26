module Server.DataStore
  ( BuildStore (..),
    BuildRecord (..),
    BuildPair (..)
  )
where

import Server.Domain

data BuildRecord = BuildRecord
  { buildId :: BuildId
  , versionId :: VersionId
  , state :: BuildState
  } deriving (Show, Eq)

data BuildPair = BuildPair
  { slowBuild :: BuildRecord
  , fastBuild :: BuildRecord
  } deriving (Show, Eq)

data BuildStore = BuildStore 
  { findBuildPair :: BuildId -> IO (Maybe BuildPair)  -- Takes a build ID, returns matching rows
  }
