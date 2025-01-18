module Config.InstallerValidator
  ( validateInstaller
  )
where

import System.Directory (doesDirectoryExist)

import Config.ApplicationConfig (ApplicationConfig (Invalid, Installer))

validateInstaller :: FilePath -> IO ApplicationConfig
validateInstaller path = do
  exists <- doesDirectoryExist path
  if exists
    then pure $ Installer path
    else pure $ Invalid "Directory does not exist"


