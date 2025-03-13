module Main where

import Config.ApplicationConfig (ApplicationConfig (..), parseApplicationConfig)
import Config.Validator (validate)
import Control.Concurrent (forkIO, killThread, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import Control.Monad (void)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setInstallShutdownHandler, setPort)
import Server (makeWaiApp)
import System.Environment (getArgs)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

main :: IO ()
main = do
  args <- getArgs
  v <- validate $ parseApplicationConfig args
  case v of
    Server port -> runServer port
    _ -> print v

closeSocketOnSIGINT :: IO () -> IO ()
closeSocketOnSIGINT closeSocket =
  void $
    installHandler
      sigINT
      ( Catch $ do
          putStrLn "\nReceived Ctrl+C, shutting down..."
          closeSocket
          -- Force exit if the server doesn't shut down cleanly
          tid <- myThreadId
          killThread tid
      )
      Nothing

runServer :: Int -> IO ()
runServer port = do
  putStrLn "Starting server... (Press Ctrl+C to stop)"
  app <- makeWaiApp "data"
  putStrLn $ "Running server on port " ++ show port

  -- Create shutdown signal
  shutdownMVar <- newEmptyMVar

  -- Install signal handler that can interrupt request processing
  void $
    installHandler
      sigINT
      ( Catch $ do
          putStrLn "\nReceived Ctrl+C, shutting down..."
          putMVar shutdownMVar ()
      )
      Nothing

  -- Run server in separate thread
  let settings = setPort port defaultSettings
  serverThread <- forkIO $ runSettings settings app

  -- Wait for shutdown signal
  takeMVar shutdownMVar
  killThread serverThread
  putStrLn "Server shutdown complete"
