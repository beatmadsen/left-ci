{-# LANGUAGE OverloadedStrings #-}

module Server.Service
  ( BuildService (..),
  )
where

import Server.Domain (BuildId, BuildState, BuildSummary, VersionId)

data BuildService = BuildService
  { getBuildSummary :: BuildId -> IO (Maybe BuildSummary),
    createBuild :: VersionId -> BuildId -> IO (),
    advanceFastResult :: BuildId -> IO (),
    advanceSlowResult :: BuildId -> IO (),
    failFastResult :: BuildId -> IO (),
    failSlowResult :: BuildId -> IO ()
  }
