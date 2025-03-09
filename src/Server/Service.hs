module Server.Service
  ( BuildService (..),
    CreationOutcome (..),
    StateChangeOutcome (..),
    BuildMap,
    parseAfter,
    advance
  )
where

import Server.Domain (Build, BuildState (..), BuildSummary, Version, Project)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Text (Text)


type BuildMap = Map.Map Build BuildSummary

data CreationOutcome = Conflict | SuccessfullyCreated deriving (Show, Eq)
data StateChangeOutcome = NotFound | SuccessfullyChangedState deriving (Show, Eq)

data BuildService = BuildService
  { getBuildSummary :: Build -> IO (Maybe BuildSummary),
    listProjectBuilds :: Project -> Maybe UTCTime -> IO (Maybe BuildMap),
    createBuild :: Project -> Version -> Build -> IO CreationOutcome,
    advanceFastSuite :: Build -> IO StateChangeOutcome,
    advanceSlowSuite :: Build -> IO StateChangeOutcome,
    failFastSuite :: Build -> IO StateChangeOutcome,
    failSlowSuite :: Build -> IO StateChangeOutcome
  }

advance :: BuildState -> BuildState
advance Init = Running
advance Running = Passed
advance s = s


parseAfter :: Text -> Either () UTCTime
parseAfter t = undefined