{-# LANGUAGE OverloadedStrings #-}

module Server.Service
  ( BuildService (..),
  )
where

import Server.Domain (BuildId, BuildState, BuildSummary, VersionId)

data BuildService = BuildService
  { getBuildSummary :: BuildId -> IO (Maybe BuildSummary),
    advanceFastResult :: VersionId -> BuildId -> IO (),
    advanceSlowResult :: VersionId -> BuildId -> IO (),
    failFastResult :: VersionId -> BuildId -> IO (),
    failSlowResult :: VersionId -> BuildId -> IO ()
  }
