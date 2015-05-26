module Version
    ( version
    , protoVersion
    ) where

import Data.Version (showVersion)
import qualified Paths_FlashBit as P (version)

version :: String
version = showVersion P.version

protoVersion :: String
protoVersion = filter (/= '.') version
