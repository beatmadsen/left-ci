module Server (
  makeScottyApp,
) where

import Server.DataStore (BuildStore)
import Server.Service (BuildService)
import Server.Service.Persistent (makePersistentService)
import Server.DataStore.SQLiteStore (makeSQLiteBuildStore)
import Server.Routes (makeApplication)
import Web.Scotty (ScottyM)

makeScottyApp :: FilePath -> IO (ScottyM ())
makeScottyApp dbSubDir = do
  service <- makeService makeSQLiteBuildStore dbSubDir
  return $ makeApplication service

makeService :: (FilePath -> IO (BuildStore ctx)) -> FilePath -> IO BuildService
makeService makeBuildStore subDir = do
  store <- makeBuildStore subDir
  return $ makePersistentService store