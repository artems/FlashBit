{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.PeerManager.State
    ( PeerManagerState(..)
    , mkPeerManagerState
    , mayIAcceptIncomingPeer
    , enqueuePeers
    , nextPackOfPeers
    ) where

import qualified Control.Monad.State as S
import Torrent


data PeerManagerState = PeerManagerState
    { _peerQueue  :: [(InfoHash, Peer)]
    }

type PeerManagerMonad a = (S.MonadState PeerManagerState m) => m a

maxPeers :: Integer
maxPeers = 10

mkPeerManagerState :: PeerManagerState
mkPeerManagerState = PeerManagerState
    { _peerQueue  = []
    }

enqueuePeers :: InfoHash -> [Peer] -> PeerManagerMonad ()
enqueuePeers infoHash peers = do
    let peers' = map (infoHash,) peers
    S.modify $ \st -> st { _peerQueue = _peerQueue st ++ peers' }

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
