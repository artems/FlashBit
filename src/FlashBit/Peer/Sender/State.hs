{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.Peer.Sender.State
    ( PeerSenderState(..)
    , QueueType
    , mkPeerSenderState
    , pushQ
    , firstQ
    , prunePieceMessage
    , prunePieceRequest
    , pruneAllPieceRequests
    ) where

import Control.Concurrent.Timer
import qualified Control.Monad.State as S
import qualified Data.Sequence as S

import Torrent
import qualified Torrent.Message as TM


data PeerSenderState = PeerSenderState
    { _queue          :: S.Seq QueueType
    , _keepAliveTimer :: TimerId
    }

type QueueType = Either TM.Message (PieceNum, PieceBlock)

type PeerSenderMonad a = (S.MonadState PeerSenderState m) => m a


mkPeerSenderState :: TimerId -> PeerSenderState
mkPeerSenderState timerId = PeerSenderState
    { _queue          = S.empty
    , _keepAliveTimer = timerId
    }


pushQ :: QueueType -> PeerSenderMonad ()
pushQ a = S.modify $ \st -> st { _queue =  a S.<| (_queue st) }


firstQ :: PeerSenderMonad (Maybe QueueType)
firstQ = do
    queue <- S.gets _queue
    case S.viewr queue of
        S.EmptyR ->
            return Nothing
        queue' S.:> message -> do
            S.modify $ \st -> st { _queue = queue' }
            return (Just message)


modifyQ :: (S.Seq QueueType -> S.Seq QueueType) -> PeerSenderMonad ()
modifyQ func = S.modify $ \st -> st { _queue = func (_queue st) }

prunePieceMessage :: PieceNum -> PieceBlock -> PeerSenderMonad ()
prunePieceMessage pieceNum block = modifyQ $ S.filter (/= Right (pieceNum, block))

prunePieceRequest :: PieceNum -> PieceBlock -> PeerSenderMonad ()
prunePieceRequest pieceNum block = modifyQ $ S.filter (/= Left (TM.Request pieceNum block))

pruneAllPieceRequests :: PeerSenderMonad ()
pruneAllPieceRequests = modifyQ $ S.filter (not . isPieceRequest)
  where
    isPieceRequest (Right _)               = True
    isPieceRequest (Left (TM.Request _ _)) = True
    isPieceRequest _                       = False
