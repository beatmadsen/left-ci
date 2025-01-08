module Config.ApplicationConfig
  ( ApplicationConfig (..),
    parseApplicationConfig,
  )
where

data ApplicationConfig
  = Server Int
  | Installer FilePath
  | Invalid String
  deriving (Show, Eq)

parseApplicationConfig :: [String] -> ApplicationConfig
parseApplicationConfig [] = Invalid "No arguments provided"
parseApplicationConfig ("--server" : rest) = parseServerConfig rest
parseApplicationConfig _ = error "Not implemented yet"

parseServerConfig :: [String] -> ApplicationConfig
parseServerConfig [] = Server 8585 -- Case for just "--server"
parseServerConfig ("--port" : rest) = case rest of
  [] -> Invalid "No port number provided to run the server on. Please add a number after --port"
  (port : _) -> Invalid $ "Invalid port number: " ++ port ++ ". Please add a number after --port"
parseServerConfig _ = Invalid "Invalid server configuration"
