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
    advanceFastResult :: BuildId -> IO StateChangeOutcome,
    advanceSlowResult :: BuildId -> IO StateChangeOutcome,
    failFastResult :: BuildId -> IO StateChangeOutcome,
    failSlowResult :: BuildId -> IO StateChangeOutcome
  }

advance :: BuildState -> BuildState
advance Init = Running
advance Running = Passed
advance s = s
