{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Domain
  ( BuildState (..),
    BuildSummary (..),
    SuiteSummary (..),
    Version (..),
    Build (..),
    Project (..),
  )
where

import Data.Aeson (ToJSON (..), ToJSONKey (..), ToJSONKeyFunction (..), object, (.=))
import Data.Aeson.Key (fromText)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

newtype Project = Project
  { getName :: Text
  }
  deriving (Show, Eq)

instance IsString Project where
  fromString :: String -> Project
  fromString = Project . fromString

newtype Version = Version
  { getCommitHash :: Text
  }
  deriving (Show, Eq)

-- Make it easy to use string literals
instance IsString Version where
  fromString :: String -> Version
  fromString = Version . fromString

instance ToJSON Version where
  toJSON (Version commitHash) = toJSON commitHash

newtype Build = Build
  { getGlobalId :: Text
  }
  deriving (Show, Eq, Ord, ToJSONKey)

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
  { fastSuite :: SuiteSummary,
    slowSuite :: SuiteSummary
  }
  deriving (Show, Eq)

instance ToJSON BuildSummary where
  toJSON (BuildSummary fastSuite slowSuite) =
    object
      [ "fast_suite" .= fastSuite,
        "slow_suite" .= slowSuite
      ]

data SuiteSummary = SuiteSummary
  { state :: BuildState,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    version :: Version
  }
  deriving (Show, Eq)

instance ToJSON SuiteSummary where
  toJSON (SuiteSummary state createdAt updatedAt version) =
    object
      [ "state" .= state,
        "created_at" .= createdAt,
        "updated_at" .= updatedAt,
        "version" .= version
      ]

instance ToJSON BuildState where
  toJSON Init = "init"
  toJSON Running = "running"
  toJSON Passed = "passed"
  toJSON Failed = "failed"
