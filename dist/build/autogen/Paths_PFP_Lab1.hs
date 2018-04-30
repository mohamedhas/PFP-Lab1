{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_PFP_Lab1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/moh/.cabal/bin"
libdir     = "/home/moh/.cabal/lib/x86_64-linux-ghc-8.0.1/PFP-Lab1-0.1.0.0"
dynlibdir  = "/home/moh/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/moh/.cabal/share/x86_64-linux-ghc-8.0.1/PFP-Lab1-0.1.0.0"
libexecdir = "/home/moh/.cabal/libexec"
sysconfdir = "/home/moh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PFP_Lab1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PFP_Lab1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PFP_Lab1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PFP_Lab1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PFP_Lab1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PFP_Lab1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
