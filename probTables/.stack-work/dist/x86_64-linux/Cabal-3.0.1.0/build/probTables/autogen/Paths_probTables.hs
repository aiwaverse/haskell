{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_probTables (
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

bindir     = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/bin"
libdir     = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/lib/x86_64-linux-ghc-8.8.3/probTables-0.1.0.0-HuncquAK5bvF0FDItkHoIV-probTables"
dynlibdir  = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/share/x86_64-linux-ghc-8.8.3/probTables-0.1.0.0"
libexecdir = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/libexec/x86_64-linux-ghc-8.8.3/probTables-0.1.0.0"
sysconfdir = "/home/aiwa/Documents/aiwita/codes/haskell/probTables/.stack-work/install/x86_64-linux/21e1f7cf9c4404012abd23e3dc80abfcbfef12b281139583970f10b336caac4b/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "probTables_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "probTables_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "probTables_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "probTables_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "probTables_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "probTables_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
