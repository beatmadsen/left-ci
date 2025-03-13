{-# LANGUAGE RankNTypes #-}

module Server.DataStore
  ( BuildStore (..),
    BuildRecord (..),
    BuildPair (..),
  )
where

import Server.DataStore.Atomic
import Server.Domain
import Data.Time (UTCTime)
data BuildRecord = BuildRecord
  { build :: Build,
    version :: Version,
    state :: BuildState,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
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

    findProject :: Project -> AtomicM ctx (Maybe Project),
    findBuildPairs :: Project -> Maybe UTCTime -> AtomicM ctx [BuildPair],
    
    createBuildUnlessExists :: Project -> Version -> Build -> AtomicM ctx (Either () ()),
    
    findFastState :: Build -> AtomicM ctx (Maybe BuildState),
    updateFastState :: Build -> BuildState -> AtomicM ctx (),
    findSlowState :: Build -> AtomicM ctx (Maybe BuildState),
    updateSlowState :: Build -> BuildState -> AtomicM ctx ()
  }
