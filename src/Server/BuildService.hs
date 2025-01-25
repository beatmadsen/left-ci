{-# LANGUAGE OverloadedStrings #-}

module Server.BuildService
  ( BuildService (..),
  )
where

import Server.Domain (BuildId, BuildState, BuildSummary, VersionId)

data BuildService = BuildService
  { getBuildSummary :: BuildId -> IO BuildSummary,
    advanceFastResult :: VersionId -> BuildId -> IO (),
    advanceSlowResult :: VersionId -> BuildId -> IO ()
  }