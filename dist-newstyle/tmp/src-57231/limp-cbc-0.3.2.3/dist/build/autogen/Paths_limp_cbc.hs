{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_limp_cbc (
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
version = Version [0,3,2,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lmp-cbc-0.3.2.3-52f18f33/bin"
libdir     = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lmp-cbc-0.3.2.3-52f18f33/lib"
dynlibdir  = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lib"
datadir    = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lmp-cbc-0.3.2.3-52f18f33/share"
libexecdir = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lmp-cbc-0.3.2.3-52f18f33/libexec"
sysconfdir = "/Users/sjoerdvisscher/.cabal/store/ghc-9.10.1-64dd/lmp-cbc-0.3.2.3-52f18f33/etc"

getBinDir     = catchIO (getEnv "limp_cbc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "limp_cbc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "limp_cbc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "limp_cbc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "limp_cbc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "limp_cbc_sysconfdir") (\_ -> return sysconfdir)



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
