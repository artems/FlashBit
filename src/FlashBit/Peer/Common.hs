module FlashBit.Peer.Common
    ( PeerHandlerMessage(..)
    ) where

import Torrent.Message

data PeerHandlerMessage
    = FromPeer Message
    | FromChokeManager Bool
    | PeerTick
