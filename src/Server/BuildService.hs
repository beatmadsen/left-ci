{-# LANGUAGE OverloadedStrings #-}

module Server.BuildService
    ( BuildService(..)
    , BuildId
    ) where

import Server.Domain (BuildSummary, BuildState)

type BuildId = String
type VersionId = String

data BuildService = BuildService 
    { getBuildSummary :: BuildId -> IO BuildSummary
    , advanceFastResult :: VersionId -> BuildId -> IO ()
    }