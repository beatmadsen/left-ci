{-# LANGUAGE OverloadedStrings #-}

module Server.Domain
    ( TestResult(..)
    , BuildStatus(..)
    ) where

import Data.Aeson (ToJSON(..), object, (.=))

data TestResult = SuccessResult | FailureResult 
    deriving (Show, Eq)

data BuildStatus = BuildStatus 
    { fastResult :: Maybe TestResult
    , slowResult :: Maybe TestResult 
    } deriving (Show, Eq)

instance ToJSON TestResult where
    toJSON SuccessResult = "success"
    toJSON FailureResult = "failure"

instance ToJSON BuildStatus where
    toJSON (BuildStatus fast slow) = object
        [ "fast" .= fast
        , "slow" .= slow
        ]