module Main where

import Config.ApplicationConfig (parseApplicationConfig, ApplicationConfig (..))
import Config.Validator (validate)
import System.Environment (getArgs)
import Server (makeWaiApp)
import Network.Wai.Handler.Warp (run, runSettings, defaultSettings, setPort, setInstallShutdownHandler)
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
import Control.Concurrent (myThreadId)
import Control.Exception (finally)
import Control.Monad (void)

main :: IO ()
main = do
  args <- getArgs
  v <- validate $ parseApplicationConfig args
  case v of
    Server port -> runServer port
    _ -> putStrLn $ show v

closeSocketOnSIGINT :: IO () -> IO ()
closeSocketOnSIGINT closeSocket = void $ installHandler sigINT (Catch $ do
    putStrLn "\nReceived Ctrl+C, shutting down..."
    closeSocket
  ) Nothing

runServer :: Int -> IO ()
runServer port = do
  putStrLn "Starting server... (Press Ctrl+C to stop)"
  app <- makeWaiApp "data"
  putStrLn $ "Running server on port " ++ show port
  
  let settings = setPort port $ 
                 setInstallShutdownHandler closeSocketOnSIGINT $
                 defaultSettings
                 
  finally 
    (runSettings settings app)
    (putStrLn "Server shutdown complete")
