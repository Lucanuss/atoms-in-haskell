{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_SDLEvent (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Lukas\\OneDrive\\Dokumente\\Tuhh\\Functional Programming\\FLight\\FritzLight [03]\\SDLEvent\\.stack-work\\install\\3122d1ef\\bin"
libdir     = "C:\\Users\\Lukas\\OneDrive\\Dokumente\\Tuhh\\Functional Programming\\FLight\\FritzLight [03]\\SDLEvent\\.stack-work\\install\\3122d1ef\\lib\\x86_64-windows-ghc-8.0.1\\SDLEvent-0.1.0.0-4c9FDX62XT68p8wGySnMkf"
datadir    = "C:\\Users\\Lukas\\OneDrive\\Dokumente\\Tuhh\\Functional Programming\\FLight\\FritzLight [03]\\SDLEvent\\.stack-work\\install\\3122d1ef\\share\\x86_64-windows-ghc-8.0.1\\SDLEvent-0.1.0.0"
libexecdir = "C:\\Users\\Lukas\\OneDrive\\Dokumente\\Tuhh\\Functional Programming\\FLight\\FritzLight [03]\\SDLEvent\\.stack-work\\install\\3122d1ef\\libexec"
sysconfdir = "C:\\Users\\Lukas\\OneDrive\\Dokumente\\Tuhh\\Functional Programming\\FLight\\FritzLight [03]\\SDLEvent\\.stack-work\\install\\3122d1ef\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SDLEvent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SDLEvent_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SDLEvent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SDLEvent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SDLEvent_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
