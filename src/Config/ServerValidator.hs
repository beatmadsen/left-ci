module Config.ServerValidator
  ( validatePort
  ) where

import Control.Exception (SomeException, try)
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    socket,
    tupleToHostAddress,
  )

import Config.ApplicationConfig (ApplicationConfig (Invalid, Server))

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