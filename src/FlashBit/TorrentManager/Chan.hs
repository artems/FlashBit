module FlashBit.TorrentManager.Chan
    ( TorrentManagerMessage(..)
    ) where

import Control.Concurrent

data TorrentManagerMessage
    = AddTorrent FilePath FilePath Bool
    | RemoveTorrent FilePath
    | Shutdown (MVar ())
