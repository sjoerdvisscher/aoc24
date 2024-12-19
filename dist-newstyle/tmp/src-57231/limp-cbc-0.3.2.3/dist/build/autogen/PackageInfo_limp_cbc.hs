{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_limp_cbc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "limp_cbc"
version :: Version
version = Version [0,3,2,3] []

synopsis :: String
synopsis = "bindings for integer linear programming solver Coin/CBC"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/amosr/limp-cbc"
