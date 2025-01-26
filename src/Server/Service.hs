{-# LANGUAGE OverloadedStrings #-}

module Server.Service
  ( BuildService (..),
    Outcome (..),
  )
where

import Server.Domain (BuildId, BuildState, BuildSummary, VersionId)

data Outcome = Conflict | Success deriving (Show, Eq)

data BuildService = BuildService
  { getBuildSummary :: BuildId -> IO (Maybe BuildSummary),
    createBuild :: VersionId -> BuildId -> IO Outcome,
    advanceFastResult :: BuildId -> IO (),
    advanceSlowResult :: BuildId -> IO (),
    failFastResult :: BuildId -> IO (),
    failSlowResult :: BuildId -> IO ()
  }
