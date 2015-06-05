module FlashBit.PeerManager.Chan
    ( PeerManagerMessage(..)
    , PeerEventMessage(..)
    ) where

import Control.Exception
import qualified Network.Socket as S

import Torrent
import FlashBit.PeerDatabase (PeerTVar)


data PeerEventMessage
    = Connected InfoHash S.SockAddr PeerTVar
    | Disconnected InfoHash S.SockAddr
    | ConnectException S.SockAddr SomeException

data PeerManagerMessage
    = NewConnection (S.Socket, S.SockAddr)
    | NewTrackerPeers InfoHash [Peer]
    | StopTorrentPeers InfoHash
