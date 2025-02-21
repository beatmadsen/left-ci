module Server.DataStore.SQLiteTypes (
  SuiteName (..),
  Execution (..),
) where

import Database.SQLite.Simple.FromField (FromField, fromField, returnError, ResultError (ConversionFailed))
import Database.SQLite.Simple.ToField (ToField, toField)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Data.String (fromString)

import Server.Domain (Build (..), BuildState (..), Version (..))

data SuiteName = Fast | Slow deriving (Show, Eq)

instance FromField SuiteName where
  fromField f = do
    text <- fromField f
    case text of
      "fast" -> return Fast
      "Fast" -> return Fast
      "slow" -> return Slow
      "Slow" -> return Slow
      other -> returnError ConversionFailed f $ 
        "Invalid suite name: " ++ show other ++ ". Expected 'fast' or 'slow'"

instance ToField SuiteName where
  toField Fast = toField "fast"
  toField Slow = toField "slow"


data Execution = Execution
  { suiteName :: SuiteName,
    buildGlobalId :: Build,
    versionCommitHash :: Version,
    executionState :: BuildState
  }
instance FromField Build where
  fromField = fmap Build . fromField

instance FromField Version where
  fromField = fmap CommitHash . fromField

instance FromField BuildState where
  fromField = fmap fromString . fromField

instance FromRow Execution where
  fromRow =
    Execution
      <$> field -- suite_name
      <*> field -- build_global_id
      <*> field -- version_commit_hash
      <*> field -- state
