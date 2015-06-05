module FlashBit.Config
    ( Config(..)
    , defaultConfig
    ) where

import Data.Word
import Torrent


data Config = Config
    { _localPort        :: Word16
    , _downloadPath     :: FilePath
    , _defaultBlockSize :: PieceBlockLength
    }

defaultConfig :: Config
defaultConfig = Config
    { _localPort        = 1369
    , _downloadPath     = "."
    , _defaultBlockSize = 16384
    }
