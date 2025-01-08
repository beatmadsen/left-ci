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
parseApplicationConfig ("--server" : rest) = parseServer rest
parseApplicationConfig _ = error "Not implemented yet"

parseServer :: [String] -> ApplicationConfig
parseServer [] = Server 8585
parseServer ("--port" : rest) = case rest of
  [] -> Invalid "No port number provided to run the server on. Please add a number after --port"
  (port : _) -> parsePort port
parseServer _ = Invalid "Invalid server configuration"

parsePort :: String -> ApplicationConfig
parsePort port = case reads port :: [(Int, String)] of
  [(n, "")] | n >= 0 && n <= 65535 -> Server n
  _ -> Invalid $ "Invalid port number: " ++ port ++ ". Please add a number after --port"
