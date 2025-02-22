module Server.DataStore.SQLiteStore.Types (
  OngoingTransaction(..)
) where

import Database.SQLite.Simple (Connection)

newtype OngoingTransaction = OngoingTransaction
  { connection :: Connection
  }