module RandomHelper(
  getUniqueDirName,
  getEphemeralPort
) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

getUniqueDirName :: IO FilePath
getUniqueDirName = do
  replicateM 10 $ do
    i <- randomRIO (0, length alphabet - 1)
    return $ alphabet !! i
  where
    alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


getEphemeralPort :: IO Int
getEphemeralPort = do
  randomRIO (49152, 65535)
