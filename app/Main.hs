module Main where

import Config.ApplicationConfig (parseApplicationConfig, ApplicationConfig (..))
import Config.Validator (validate)
import System.Environment (getArgs)
import Server (makeWaiApp)
import Network.Wai.Handler.Warp (run, runSettings, defaultSettings, setPort, setInstallShutdownHandler)
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
import Control.Concurrent (myThreadId, killThread)
import qualified Control.Exception as E
import Control.Monad (void)

main :: IO ()
main = do
  args <- getArgs
  v <- validate $ parseApplicationConfig args
  case v of
    Server port -> runServer port
    _ -> print v

closeSocketOnSIGINT :: IO () -> IO ()
closeSocketOnSIGINT closeSocket = void $ installHandler sigINT (Catch $ do
    putStrLn "\nReceived Ctrl+C, shutting down..."
    closeSocket
    -- Force exit if the server doesn't shut down cleanly
    tid <- myThreadId
    killThread tid
  ) Nothing

runServer :: Int -> IO ()
runServer port = do
  putStrLn "Starting server... (Press Ctrl+C to stop)"
  app <- makeWaiApp "data"
  putStrLn $ "Running server on port " ++ show port
  
  let settings = setPort port $ 
                 setInstallShutdownHandler closeSocketOnSIGINT defaultSettings
                 
  E.bracket
    (pure ())
    (\_ -> putStrLn "Server shutdown complete")
    (\_ -> runSettings settings app)
