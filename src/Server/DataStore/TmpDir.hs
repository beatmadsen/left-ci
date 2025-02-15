module Server.DataStore.TmpDir where

import System.Directory
import System.FilePath ((</>))

tmpDir :: String -> IO FilePath
tmpDir subdir = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "left-ci" </> subdir
  createDirectoryIfMissing True path
  return path
