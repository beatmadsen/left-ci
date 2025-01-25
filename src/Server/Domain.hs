{-# LANGUAGE OverloadedStrings #-}

module Server.Domain
  ( BuildState (..),
    BuildSummary (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))

data BuildState = Init | Running | Passed | Failed
  deriving (Show, Eq)

data BuildSummary = BuildSummary
  { fastState :: BuildState,
    slowState :: BuildState
  }
  deriving (Show, Eq)

instance ToJSON BuildState where
  toJSON Init = "init"
  toJSON Running = "running"
  toJSON Passed = "passed"
  toJSON Failed = "failed"

instance ToJSON BuildSummary where
  toJSON (BuildSummary fast slow) =
    object
      [ "fast" .= fast,
        "slow" .= slow
      ]