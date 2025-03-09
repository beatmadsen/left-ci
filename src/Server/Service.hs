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
import Data.Text (Text, unpack)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import Network.HTTP.Types.URI (urlDecode)
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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


parseAfter :: Text -> Either String UTCTime
parseAfter = parseIso8601 . convertUrlEncoded  
  where
    convertUrlEncoded :: Text -> String
    convertUrlEncoded = unpack . decodeUtf8 . urlDecode True . encodeUtf8

    parseIso8601 :: String -> Either String UTCTime
    parseIso8601 str = 
      let mTime  = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" str
      in case mTime of
        Just time -> Right time
        Nothing -> Left str
