{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_project (
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
bindir     = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/bin"
libdir     = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0-CC5ng7wihT29SSkE1UM92A"
dynlibdir  = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0"
libexecdir = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/haskell-project-0.1.0.0"
sysconfdir = "/Users/mannatsingh/Downloads/Haskell-Project/.stack-work/install/aarch64-osx/c4d8c838009dc860245c5c66ce09eee1c7c65c62acd5d70435cc4e7e505da025/9.10.3/etc"

getBinDir     = catchIO (getEnv "haskell_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_project_sysconfdir") (\_ -> return sysconfdir)



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
