{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_filesTest (
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

bindir     = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/bin"
libdir     = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/lib/x86_64-linux-ghc-8.6.5/filesTest-0.1.0.0-FpqZArDuKPhFLPd0v8wwja-filesTest"
dynlibdir  = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/share/x86_64-linux-ghc-8.6.5/filesTest-0.1.0.0"
libexecdir = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/libexec/x86_64-linux-ghc-8.6.5/filesTest-0.1.0.0"
sysconfdir = "/home/aiwa/Documents/aiwita/codes/haskell/filesTest/.stack-work/install/x86_64-linux/8b66efb808105e2ca9fa5a655d3274393bce6d5da7dad99b35b511da71bae7e2/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "filesTest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "filesTest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "filesTest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "filesTest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "filesTest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "filesTest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
