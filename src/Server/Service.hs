{-# LANGUAGE OverloadedStrings #-}

module Server.Service
  ( BuildService (..),
    CreationOutcome (..),
    StateChangeOutcome (..),
    advance
  )
where

import Server.Domain (BuildId, BuildState (..), BuildSummary, VersionId)

data CreationOutcome = Conflict | SuccessfullyCreated deriving (Show, Eq)
data StateChangeOutcome = NotFound | SuccessfullyChangedState deriving (Show, Eq)

data BuildService = BuildService
  { getBuildSummary :: BuildId -> IO (Maybe BuildSummary),
    createBuild :: VersionId -> BuildId -> IO CreationOutcome,
    advanceFastSuite :: BuildId -> IO StateChangeOutcome,
    advanceSlowSuite :: BuildId -> IO StateChangeOutcome,
    failFastSuite :: BuildId -> IO StateChangeOutcome,
    failSlowSuite :: BuildId -> IO StateChangeOutcome
  }

advance :: BuildState -> BuildState
advance Init = Running
advance Running = Passed
advance s = s
