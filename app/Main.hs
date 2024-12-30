module Main where

import Server.App (runWithArgsAndServer)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = runWithArgsAndServer [] (run 3000)
