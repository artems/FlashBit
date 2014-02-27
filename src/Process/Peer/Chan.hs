module Process.Peer.Chan
    ( PeerMessage(..)
    ) where


import Protocol.Peer


data PeerMessage
    = PeerHandshake Handshake
    | PeerMessage Message
