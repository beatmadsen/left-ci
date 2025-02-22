{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Server.Domain
  ( BuildState (..),
    BuildSummary (..),
    Version(..),
    Build(..),
    Project(..)
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.String (IsString(..))
import Data.Text (Text)

newtype Project = Project {
  getName :: Text
} deriving (Show, Eq)

instance IsString Project where
    fromString :: String -> Project
    fromString = Project . fromString

newtype Version = Version { 
  getCommitHash :: Text 
} deriving (Show, Eq)

-- Make it easy to use string literals
instance IsString Version where
    fromString :: String -> Version
    fromString = Version . fromString

newtype Build = Build {
  getGlobalId :: Text
} deriving (Show, Eq, Ord)

instance IsString Build where
    fromString :: String -> Build
    fromString = Build . fromString

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