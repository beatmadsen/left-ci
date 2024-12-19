{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_left_ci (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/bin"
libdir     = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/lib/aarch64-osx-ghc-9.10.1-64dd/left-ci-0.1.0.0-4oTC30DLZZqawZcUT3MQZ"
dynlibdir  = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/lib/aarch64-osx-ghc-9.10.1-64dd"
datadir    = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/share/aarch64-osx-ghc-9.10.1-64dd/left-ci-0.1.0.0"
libexecdir = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/libexec/aarch64-osx-ghc-9.10.1-64dd/left-ci-0.1.0.0"
sysconfdir = "/Users/erik.thyge.madsen/Developer/hobby/left-ci/new-attempt/.stack-work/install/aarch64-osx/d79cc712207da4a9560e9803369214d072764e9dfcabc97362f2234723f09124/9.10.1/etc"

getBinDir     = catchIO (getEnv "left_ci_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "left_ci_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "left_ci_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "left_ci_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "left_ci_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "left_ci_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
