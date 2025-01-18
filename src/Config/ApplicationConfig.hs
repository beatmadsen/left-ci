module Config.ApplicationConfig
  ( ApplicationConfig (..),
    parseApplicationConfig,
    validate
  )
where

import Network.Socket
  ( Socket,
    bind,
    close,
    defaultProtocol,
    socket,
    tupleToHostAddress,
    SockAddr(SockAddrInet),
    Family(AF_INET),
    SocketType(Stream)
  )
import Control.Exception (try, SomeException)

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

validate :: ApplicationConfig -> IO ApplicationConfig
validate (Invalid i) = pure $ Invalid i
validate (Server port) = validatePort port
validate _ = undefined

bindPort :: Socket -> Int -> IO ()
bindPort sock port = do
  bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  close sock
  pure ()

validatePort :: Int -> IO ApplicationConfig
validatePort port = do
  sock <- socket AF_INET Stream defaultProtocol
  result <- try $ bindPort sock port :: IO (Either SomeException ())
  case result of
    Left _ -> pure $ Invalid "Server port is not available"
    Right _ -> pure $ Server port
