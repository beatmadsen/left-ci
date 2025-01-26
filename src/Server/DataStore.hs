module Server.DataStore
  ( BuildStore (..),
    BuildRow (..)
  )
where

import Data.Text (Text)

data BuildRow = BuildRow
  { buildId :: Text
  , versionId :: Text
  , cadence :: Text  -- "slow" or "fast"
  , state :: Text    -- raw state string from DB
  }

data BuildStore = BuildStore 
  { getBuildRows :: Text -> IO [BuildRow]  -- Takes a build ID, returns matching rows
  }
