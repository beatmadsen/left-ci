{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Routes
    ( makeApplication
    ) where

import Web.Scotty (ScottyM, get, post, json, pathParam, request)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), withObject, (.:), decode)
import Data.Aeson.Types (Parser)
import Server.BuildService (BuildService(..), BuildId)
import Server.Domain (TestResult(..))
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai (getRequestBodyChunk)

data ResultRequest = ResultRequest 
    { result :: TestResult 
    }

instance FromJSON ResultRequest where
    parseJSON = withObject "ResultRequest" $ \v -> do
        resultStr <- v .: "result" :: Parser Text
        pure $ ResultRequest $ case resultStr of
            "success" -> SuccessResult
            _ -> FailureResult

makeApplication :: BuildService -> ScottyM ()
makeApplication service = do
    get "/build/:id" $ do
        buildId <- pathParam "id"
        status <- liftIO $ getBuildStatus service buildId
        json status
        
    post "/build/:id/fast" $ do
        buildId <- pathParam "id"
        waiRequest <- request
        chunk <- liftIO $ getRequestBodyChunk waiRequest
        case decode (LBS.fromStrict chunk) of
            Nothing -> error "Failed to parse JSON"
            Just (ResultRequest{result}) -> do
                liftIO $ setFastResult service buildId result
                json ()