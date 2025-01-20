{-# LANGUAGE OverloadedStrings #-}

module Server.BuildService
    ( BuildService(..)
    , BuildId
    ) where

import Server.Domain (BuildStatus, TestResult)

type BuildId = String

data BuildService = BuildService 
    { getBuildStatus :: BuildId -> IO BuildStatus
    , setFastResult :: BuildId -> TestResult -> IO ()
    }