{-# LANGUAGE OverloadedStrings #-}

module Server.Types 
    ( TestResult(..)
    , BuildStatus(..)
    , BuildService(..)
    ) where

import Data.Aeson (ToJSON(..), object, (.=))

data TestResult = SuccessResult | FailureResult 
    deriving (Show, Eq)

data BuildStatus = BuildStatus 
    { fastResult :: Maybe TestResult
    , slowResult :: Maybe TestResult 
    } deriving (Show, Eq)

data BuildService = BuildService 
    { getBuildStatus :: String -> IO BuildStatus
    }

instance ToJSON TestResult where
    toJSON SuccessResult = "success"
    toJSON FailureResult = "failure"

instance ToJSON BuildStatus where
    toJSON (BuildStatus fast slow) = object
        [ "fast" .= fast
        , "slow" .= slow
        ]