module FlashBit.TorrentManager.Chan
    ( TorrentManagerMessage(..)
    ) where

import Torrent
import Torrent.File

data TorrentManagerMessage
    = AddTorrent Torrent (FileRec, PieceHaveMap) Bool
    | StopTorrent InfoHash
    | StartTorrent InfoHash
    | RemoveTorrent InfoHash
