{-# LANGUAGE OverloadedStrings #-}

module Server.Service
  ( BuildService (..),
    CreationOutcome (..),
    StateChangeOutcome (..),
    advance
  )
where

import Server.Domain (Build, BuildState (..), BuildSummary, Version)

data CreationOutcome = Conflict | SuccessfullyCreated deriving (Show, Eq)
data StateChangeOutcome = NotFound | SuccessfullyChangedState deriving (Show, Eq)

data BuildService = BuildService
  { getBuildSummary :: Build -> IO (Maybe BuildSummary),
    createBuild :: Version -> Build -> IO CreationOutcome,
    advanceFastSuite :: Build -> IO StateChangeOutcome,
    advanceSlowSuite :: Build -> IO StateChangeOutcome,
    failFastSuite :: Build -> IO StateChangeOutcome,
    failSlowSuite :: Build -> IO StateChangeOutcome
  }

advance :: BuildState -> BuildState
advance Init = Running
advance Running = Passed
advance s = s
