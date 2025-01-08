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
parseApplicationConfig ("--installer" : rest) = parseInstaller rest
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

parseInstaller :: [String] -> ApplicationConfig
parseInstaller [] = Invalid "No path provided to the project where you want to install Left CI. Please add a path after --installer using --path <path>"
parseInstaller ("--path" : rest) = case rest of
  [] -> Invalid "No path provided to the project where you want to install Left CI. Please add a path after --installer using --path <path>"
  (path : _) -> Installer path
parseInstaller _ = Invalid "Invalid installer configuration"
