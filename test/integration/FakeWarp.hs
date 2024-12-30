{-# LANGUAGE OverloadedStrings #-}
module FakeWarp
    ( fakeWarp
    ) where

import Network.Wai
    ( defaultRequest,
      Application,
      Request(pathInfo),
      Response,
      ResponseReceived )
import Network.Wai.Internal (ResponseReceived(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Server.App (AppRunner)

fakeWarp :: AppRunner
fakeWarp app = do
    putStrLn "Fake server running"
    runFakeRequests app ["/hello", "/goodbye", "/echo?msg=test"]
  where
    runFakeRequests :: Application -> [Text] -> IO ()
    runFakeRequests _ [] = putStrLn "All requests processed."
    runFakeRequests app (path : paths) = do
        let fakeRequest = defaultRequest { pathInfo = parsePath path }
        app fakeRequest fakeResponder
        runFakeRequests app paths

    fakeResponder :: Response -> IO ResponseReceived
    fakeResponder response = do
        putStrLn "Received response"
        pure ResponseReceived

    parsePath :: Text -> [Text]
    parsePath path = case Text.split (== '/') path of
        ("":xs) -> xs
        xs -> xs
