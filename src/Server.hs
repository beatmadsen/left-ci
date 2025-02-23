module Server (
  makeWaiApp,
) where

import Server.DataStore (BuildStore)
import Server.Service (BuildService)
import Server.Service.Persistent (makePersistentService)
import Server.DataStore.SQLiteStore (makeSQLiteBuildStore)
import Server.Routes (makeApplication)
import Web.Scotty (scottyApp)
import Network.Wai (Application)
import qualified Paths_left_ci as P

makeWaiApp :: FilePath -> IO Application
makeWaiApp dbSubDir = do
  service <- makeService makeSQLiteBuildStore dbSubDir
  dataDir <- P.getDataDir
  scottyApp $ makeApplication dataDir service

makeService :: (FilePath -> IO (BuildStore ctx)) -> FilePath -> IO BuildService
makeService makeBuildStore subDir = do
  store <- makeBuildStore subDir
  return $ makePersistentService store