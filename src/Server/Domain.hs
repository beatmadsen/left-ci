{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Server.Domain
  ( BuildState (..),
    BuildSummary (..),
    VersionId(..),
    BuildId(..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.String (IsString(..))
import Data.Text (Text)

newtype VersionId = VersionId Text
  deriving (Show, Eq)

-- Make it easy to use string literals
instance IsString VersionId where
    fromString :: String -> VersionId
    fromString = VersionId . fromString

newtype BuildId = BuildId Text
  deriving (Show, Eq)

instance IsString BuildId where
    fromString :: String -> BuildId
    fromString = BuildId . fromString

data BuildState = Init | Running | Passed | Failed
  deriving (Show, Eq)

instance IsString BuildState where
    fromString :: String -> BuildState
    fromString "init" = Init
    fromString "Init" = Init
    fromString "running" = Running
    fromString "Running" = Running
    fromString "passed" = Passed
    fromString "Passed" = Passed
    fromString "failed" = Failed
    fromString "Failed" = Failed

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