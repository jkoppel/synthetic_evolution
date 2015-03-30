module Paths_gensketch (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jkoppel/.cabal/bin"
libdir     = "/Users/jkoppel/.cabal/lib/x86_64-osx-ghc-7.8.3/gensketch-0.1.0.0"
datadir    = "/Users/jkoppel/.cabal/share/x86_64-osx-ghc-7.8.3/gensketch-0.1.0.0"
libexecdir = "/Users/jkoppel/.cabal/libexec"
sysconfdir = "/Users/jkoppel/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gensketch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gensketch_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gensketch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gensketch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gensketch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
