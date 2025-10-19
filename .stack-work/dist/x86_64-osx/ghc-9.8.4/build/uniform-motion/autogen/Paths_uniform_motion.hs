{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_uniform_motion (
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
bindir     = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/bin"
libdir     = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/lib/x86_64-osx-ghc-9.8.4/uniform-motion-0.1.0.0-3uek1cEEL64FUTBWrUJz9X-uniform-motion"
dynlibdir  = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/lib/x86_64-osx-ghc-9.8.4"
datadir    = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/share/x86_64-osx-ghc-9.8.4/uniform-motion-0.1.0.0"
libexecdir = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/libexec/x86_64-osx-ghc-9.8.4/uniform-motion-0.1.0.0"
sysconfdir = "/Users/nakamurashunsuke/Documents/\20339\32724/uniform-motion/.stack-work/install/x86_64-osx/1654755b45d81250b53f8b058526f37bb22463645af357fe8004be7388f96dee/9.8.4/etc"

getBinDir     = catchIO (getEnv "uniform_motion_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "uniform_motion_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "uniform_motion_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "uniform_motion_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "uniform_motion_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "uniform_motion_sysconfdir") (\_ -> return sysconfdir)



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
