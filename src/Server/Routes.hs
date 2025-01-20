{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Routes
    ( makeApplication
    ) where

import Web.Scotty 
    ( ScottyM, get, post, json, jsonData, pathParam, ActionM, status
    , catch)
import Web.Scotty.Internal.Types (ActionError)
import Network.HTTP.Types.Status (status500)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Server.BuildService (BuildService(..), BuildId)
import Server.Domain (TestResult(..))
import Data.Text (Text)

data ResultRequest = ResultRequest 
    { result :: TestResult 
    }

instance FromJSON ResultRequest where
    parseJSON = withObject "ResultRequest" $ \v -> do
        resultStr <- v .: "result" :: Parser Text
        pure $ ResultRequest $ case resultStr of
            "success" -> SuccessResult
            _ -> FailureResult

handleJsonError :: ActionError -> ActionM a
handleJsonError e = do
    status status500
    json $ "Failed to parse JSON: " ++ show e
    error "JSON parse failed"

parseResultRequest :: ActionM ResultRequest
parseResultRequest = jsonData `catch` handleJsonError

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
    get "/build/:id" $ do
        buildId <- pathParam "id"
        status <- liftIO $ getBuildStatus service buildId
        json status
        
    post "/build/:id/fast" $ do
        buildId <- pathParam "id"
        request <- parseResultRequest
        liftIO $ setFastResult service buildId (result request)
        json ()