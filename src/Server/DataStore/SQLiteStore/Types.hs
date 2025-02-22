module Server.DataStore.SQLiteStore.Types (
  OngoingTransaction(..),
  SuiteName (..),
  Execution (..),
) where

import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple.FromField (FromField, fromField, returnError, ResultError (ConversionFailed))
import Database.SQLite.Simple.ToField (ToField, toField)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow(..), toRow)
import Data.String (fromString)
import Server.Domain (Build (..), BuildState (..), Version (..), Project (..))


newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }


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
  fromField = fmap Version . fromField

instance ToField Project where
  toField (Project name) = toField name

instance ToRow Project where
  toRow (Project name) = toRow [name]

instance FromField Project where
  fromField = fmap Project . fromField

instance FromField BuildState where
  fromField = fmap fromString . fromField

instance FromRow Execution where
  fromRow =
    Execution
      <$> field -- suite_name
      <*> field -- build_global_id
      <*> field -- version_commit_hash
      <*> field -- state
