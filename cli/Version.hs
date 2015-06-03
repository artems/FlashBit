module Version
    ( version
    , protoVersion
    ) where

import Data.Version
import qualified Paths_FlashBit as P (version)

version :: String
version = showVersion P.version

protoVersion :: String
protoVersion = concatMap show . take 4 . versionBranch $ P.version
