module Torrent.Peer
    ( Peer(..)
    , PeerId
    , PeerStatus(..)
    , Capability(..)
    ) where

import qualified Network.Socket as S


data Peer = Peer S.SockAddr
    deriving (Eq, Show)

type PeerId = String

data PeerStatus = Seeder | Leecher
    deriving (Eq, Show)

data Capability = Fast | Extended
    deriving (Eq, Show)
