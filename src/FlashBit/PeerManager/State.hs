{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.PeerManager.State
    ( PeerManagerState(..)
    , mkPeerManagerState
    , mayIAcceptIncomingPeer
    , enqueuePeers
    , nextPackOfPeers
    , addPendingPeer
    , removePendingPeer
    , pendingPeersSize
    , clearQueue
    ) where

import qualified Data.Set as S
import qualified Control.Monad.State as S
import qualified Network.Socket as S
import Torrent


data PeerManagerState = PeerManagerState
    { _peerQueue   :: [(InfoHash, Peer)]
    , _peerPending :: S.Set S.SockAddr
    }

type PeerManagerMonad a = (S.MonadState PeerManagerState m) => m a

maxPeers :: Integer
maxPeers = 5

mkPeerManagerState :: PeerManagerState
mkPeerManagerState = PeerManagerState
    { _peerQueue   = []
    , _peerPending = S.empty
    }

clearQueue :: InfoHash -> PeerManagerMonad ()
clearQueue infoHash =
    S.modify $ \st -> st { _peerQueue = filter' (_peerQueue st) }
  where
    filter' queue = filter (\(infoHash', _) -> infoHash /= infoHash') queue

enqueuePeers :: InfoHash -> [Peer] -> PeerManagerMonad ()
enqueuePeers infoHash peers = do
    let peers' = map (infoHash,) peers
    S.modify $ \st -> st { _peerQueue = _peerQueue st ++ peers' }

addPendingPeer :: S.SockAddr -> PeerManagerMonad ()
addPendingPeer sockaddr =
    S.modify $ \st -> st { _peerPending = S.insert sockaddr (_peerPending st) }

removePendingPeer :: S.SockAddr -> PeerManagerMonad ()
removePendingPeer sockaddr =
    S.modify $ \st -> st { _peerPending = S.delete sockaddr (_peerPending st) }

pendingPeersSize :: PeerManagerMonad Integer
pendingPeersSize = (fromIntegral . S.size) `S.liftM` S.gets _peerPending

nextPackOfPeers :: Integer -> PeerManagerMonad [(InfoHash, Peer)]
nextPackOfPeers count = do
    if (count < maxPeers)
        then do
            queue <- S.gets _peerQueue
            let (peers, remain) = splitAt (fromIntegral (maxPeers - count)) queue
            S.modify $ \s -> s { _peerQueue = remain }
            return peers
        else
            return []

mayIAcceptIncomingPeer :: Integer -> PeerManagerMonad Bool
mayIAcceptIncomingPeer count = do
    return (count < maxPeers)
