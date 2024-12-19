module Engine
    ( Mode(..)
    , parseMode
    ) where

data Mode = ServerMode
    deriving (Show, Eq)

parseMode :: [String] -> IO Mode
parseMode ["server"] = pure ServerMode
parseMode _ = error "Invalid mode"
