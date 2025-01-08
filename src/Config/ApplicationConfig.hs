module Config.ApplicationConfig
  ( ApplicationConfig(..)
  , parseApplicationConfig
  ) where

data ApplicationConfig
  = Server Int
  | Installer FilePath
  | Invalid String
  deriving (Show, Eq)

parseApplicationConfig :: [String] -> ApplicationConfig

parseApplicationConfig [] = Invalid "No arguments provided"
parseApplicationConfig ["--server"] = Invalid "No port number provided to run the server on. Please add --port <port>"
parseApplicationConfig _ = error "Not implemented yet"
