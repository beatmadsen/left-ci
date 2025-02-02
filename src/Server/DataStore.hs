{-# LANGUAGE RankNTypes #-}

module Server.DataStore
  ( BuildStore (..),
    BuildRecord (..),
    BuildPair (..),
  )
where

import Server.DataStore.Atomic
import Server.Domain

data BuildRecord = BuildRecord
  { buildId :: BuildId,
    versionId :: VersionId,
    state :: BuildState
  }
  deriving (Show, Eq)

data BuildPair = BuildPair
  { slowBuild :: BuildRecord,
    fastBuild :: BuildRecord
  }
  deriving (Show, Eq)

data BuildStore ctx = BuildStore
  { findBuildPair :: BuildId -> AtomicM ctx (Maybe BuildPair),
    createBuildUnlessExists :: BuildId -> VersionId -> AtomicM ctx (Either () ()),
    atomically :: forall a. AtomicM ctx a -> IO a,
    findFastState :: BuildId -> AtomicM ctx (Maybe BuildState),
    updateFastState :: BuildId -> BuildState -> AtomicM ctx (),
    findSlowState :: BuildId -> AtomicM ctx (Maybe BuildState),
    updateSlowState :: BuildId -> BuildState -> AtomicM ctx ()
  }
