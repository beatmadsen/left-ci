module Server.DataStore.SQLiteTypes (
  SuiteName (..),
) where

import Database.SQLite.Simple.FromField (FromField, fromField, returnError, ResultError (ConversionFailed))
import Database.SQLite.Simple.ToField (ToField, toField)
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
