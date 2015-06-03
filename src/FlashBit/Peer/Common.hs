module FlashBit.Peer.Common
    ( PeerMessage(..)
    ) where

import Torrent.Message as TM

data PeerMessage
    = FromPeer TM.Message
    | FromChokeManager Bool
    | Tick
