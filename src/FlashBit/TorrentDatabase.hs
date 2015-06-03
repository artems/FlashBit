module FlashBit.TorrentDatabase
    ( TorrentTVar
    , TorrentState(..)
    , TorrentChannel(..)
    , TorrentDatabaseTVar
    , mkTorrentState
    , mkTorrentStateSTM
    , mkTorrentDatabaseSTM
    , addTorrentSTM
    , removeTorrentSTM
    , doesTorrentExistSTM
    , transferredUpdateSTM
    , pieceCompletedSTM
    , torrentCompletedSTM
    , trackerUpdatedSTM
    , getTorrentSTM
    , getStatusSTM
    , isTorrentCompleteSTM
    , getStatisticSTM
    ) where

import qualified Data.Map as M
import Control.Monad (liftM)
import Control.Concurrent.STM

import Torrent
import qualified FlashBit.FileAgent as FileAgent
import qualified FlashBit.PieceManager.Chan as PieceManager
import qualified FlashBit.Tracker.Chan as Tracker


data TorrentState = TorrentState
    { _torrentInfoHash   :: InfoHash
    , _torrentPieceArray :: PieceArray
    , _torrentStatus     :: TorrentStatus
    , _torrentChannel    :: TorrentChannel
    }

data TorrentChannel = TorrentChannel
    { _torrentTrackerChan        :: TChan Tracker.TrackerMessage
    , _torrentFileAgentChan      :: TChan FileAgent.FileAgentMessage
    , _torrentPieceManagerChan   :: TChan PieceManager.PieceManagerMessage
    , _torrentPieceBroadcastChan :: TChan PieceManager.PieceBroadcastMessage
    }

type TorrentTVar = TVar TorrentState

type TorrentDatabase = M.Map InfoHash TorrentTVar

type TorrentDatabaseTVar = TVar TorrentDatabase

mkTorrentState :: InfoHash -> PieceArray -> Integer -> TorrentChannel -> TorrentState
mkTorrentState infoHash pieceArray left torrentChannel =
        TorrentState
            { _torrentInfoHash   = infoHash
            , _torrentPieceArray = pieceArray
            , _torrentStatus     = mkTorrentStatus left
            , _torrentChannel    = torrentChannel
            }

mkTorrentStateSTM :: InfoHash -> PieceArray -> Integer -> TorrentChannel -> STM TorrentTVar
mkTorrentStateSTM infoHash pieceArray left torrentChannel =
    newTVar $ mkTorrentState infoHash pieceArray left torrentChannel

mkTorrentDatabase :: TorrentDatabase
mkTorrentDatabase = M.empty

mkTorrentDatabaseSTM :: STM TorrentDatabaseTVar
mkTorrentDatabaseSTM = newTVar mkTorrentDatabase

mkTorrentStatus :: Integer -> TorrentStatus
mkTorrentStatus left =
    TorrentStatus
        { _torrentLeft       = left
        , _torrentUploaded   = 0
        , _torrentDownloaded = 0
        , _torrentComplete   = Nothing
        , _torrentIncomplete = Nothing
        , _torrentPeerStatus = if left == 0 then Seeding else Leeching
        }

modify :: TVar a -> (a -> a) -> STM ()
modify tv f = do
    db <- readTVar tv
    writeTVar tv (f db)

addTorrentSTM :: TorrentDatabaseTVar -> TorrentTVar -> STM ()
addTorrentSTM tv torrentTV = do
    torrent <- readTVar torrentTV
    modify tv $ M.insert (_torrentInfoHash torrent) torrentTV

removeTorrentSTM :: TorrentDatabaseTVar -> InfoHash -> STM ()
removeTorrentSTM tv infoHash = modify tv $ M.delete infoHash

doesTorrentExistSTM :: TorrentDatabaseTVar -> InfoHash -> STM Bool
doesTorrentExistSTM tv infoHash = M.member infoHash `liftM` readTVar tv

getTorrentSTM :: TorrentDatabaseTVar -> InfoHash -> STM (Maybe TorrentTVar)
getTorrentSTM tv infoHash = M.lookup infoHash `liftM` readTVar tv

getStatisticSTM :: TorrentDatabaseTVar -> STM [(InfoHash, TorrentStatus)]
getStatisticSTM tv = do
    db <- readTVar tv
    mapM extractStatus (M.toList db)
  where
    extractStatus (infoHash, torrentTV) = do
        torrent <- readTVar torrentTV
        return (infoHash, _torrentStatus torrent)

adjustStatus :: TorrentTVar -> (TorrentStatus -> TorrentStatus) -> STM ()
adjustStatus tv adjuster = modify tv $ \torrent ->
    torrent { _torrentStatus = adjuster (_torrentStatus torrent) }

getStatusSTM :: TorrentTVar -> STM TorrentStatus
getStatusSTM tv = _torrentStatus `liftM` readTVar tv

isTorrentCompleteSTM :: TorrentTVar -> STM Bool
isTorrentCompleteSTM tv = ((== Seeding) . _torrentPeerStatus) `liftM` getStatusSTM tv

transferredUpdateSTM :: TorrentTVar -> UpDownStat -> STM ()
transferredUpdateSTM tv upDown = do
    adjustStatus tv $ \torrent -> torrent
        { _torrentUploaded   = _torrentUploaded torrent + _statUploaded upDown
        , _torrentDownloaded = _torrentDownloaded torrent + _statDownloaded upDown
        }

pieceCompletedSTM :: TorrentTVar -> Integer -> STM ()
pieceCompletedSTM tv bytes =
    adjustStatus tv $ \rec -> rec { _torrentLeft = _torrentLeft rec - bytes }

torrentCompletedSTM :: TorrentTVar -> STM ()
torrentCompletedSTM tv =
    adjustStatus tv $ \rec -> rec { _torrentPeerStatus = Seeding }

trackerUpdatedSTM :: TorrentTVar -> Maybe Integer -> Maybe Integer -> STM ()
trackerUpdatedSTM tv complete incomplete =
    adjustStatus tv $ \torrent -> torrent
        { _torrentComplete   = complete
        , _torrentIncomplete = incomplete
        }
