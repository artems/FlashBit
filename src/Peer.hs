module Peer
    ( Peer(..)
    , PeerId
    , InfoHash
    , Capabilities(..)
    ) where


import qualified Data.ByteString as B
import Network.Socket (SockAddr)


data Peer = Peer SockAddr
    deriving (Show)

type PeerId = String

type InfoHash = B.ByteString

data Capabilities = Fast | Extended
    deriving (Show)
