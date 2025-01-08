module Config.ApplicationConfig
  ( ApplicationConfig(..)
  , parseApplicationConfig
  ) where

data ApplicationConfig
  = Server Int
  | Installer FilePath
  | Invalid
  deriving (Show, Eq)

parseApplicationConfig :: [String] -> ApplicationConfig
parseApplicationConfig _ = error "Not implemented yet"
