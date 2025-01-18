module Config.Validator
  ( validate
  )
where


import Config.ApplicationConfig (ApplicationConfig (Invalid, Installer, Server))
import Config.ServerValidator (validatePort)
import Config.InstallerValidator (validateInstaller)

validate :: ApplicationConfig -> IO ApplicationConfig
validate (Invalid i) = pure $ Invalid i
validate (Server port) = validatePort port
validate (Installer path) = validateInstaller path
