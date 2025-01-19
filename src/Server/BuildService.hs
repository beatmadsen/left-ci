{-# LANGUAGE OverloadedStrings #-}

module Server.BuildService
    ( BuildService(..)
    ) where

import Server.Domain (BuildStatus)

data BuildService = BuildService 
    { getBuildStatus :: String -> IO BuildStatus
    }