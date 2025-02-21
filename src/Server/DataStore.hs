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
  { buildId :: Build,
    versionId :: Version,
    state :: BuildState
  }
  deriving (Show, Eq)

data BuildPair = BuildPair
  { slowSuite :: BuildRecord,
    fastSuite :: BuildRecord
  }
  deriving (Show, Eq)

data BuildStore ctx = BuildStore
  { atomically :: forall a. AtomicM ctx a -> IO a,
    -- TODO: does not need to be atomic
    findBuildPair :: Build -> AtomicM ctx (Maybe BuildPair),
    findBuildPairs :: Project -> AtomicM ctx [BuildPair],
    
    createBuildUnlessExists :: Project -> Version -> Build -> AtomicM ctx (Either () ()),
    
    findFastState :: Build -> AtomicM ctx (Maybe BuildState),
    updateFastState :: Build -> BuildState -> AtomicM ctx (),
    findSlowState :: Build -> AtomicM ctx (Maybe BuildState),
    updateSlowState :: Build -> BuildState -> AtomicM ctx ()
  }
