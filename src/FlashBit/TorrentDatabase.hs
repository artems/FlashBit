module FlashBit.TorrentDatabase
    ( TorrentTVar
    , TorrentState(..)
    , TorrentChannel(..)
    , TorrentDatabaseTVar
    , mkTorrentStateSTM
    , mkTorrentDatabaseSTM
    , torrentMapSTM
    , torrentMapSTM_
    , addTorrentSTM
    , removeTorrentSTM
    , trackerMessageSTM
    , doesTorrentExistSTM
    , transferredUpdateSTM
    , pieceCompletedSTM
    , torrentCompletedSTM
    , trackerUpdatedSTM
    , getTorrentSTM
    , getActiveStateSTM
    , getStatusSTM
    , isTorrentCompleteSTM
    , getInfoHashSTM
    , getTorrentsSTM
    , getStatisticSTM
    , torrentChangeActiveStateSTM
    ) where

import qualified Data.Map as M
import qualified Data.Traversable as T
import Control.Concurrent.STM

import Torrent
import qualified FlashBit.FileAgent as FileAgent
import qualified FlashBit.Tracker.Chan as Tracker
import qualified FlashBit.PieceManager.Chan as PieceManager


data TorrentState = TorrentState
    { _torrent           :: Torrent
    , _torrentStatus     :: TorrentStatus
    , _torrentChannel    :: TorrentChannel
    , _torrentActive     :: Bool
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

mkTorrentState :: Torrent -> Integer -> TorrentChannel -> Bool -> TorrentState
mkTorrentState torrent left torrentChannel active =
    TorrentState
        { _torrent           = torrent
        , _torrentStatus     = mkTorrentStatus left
        , _torrentChannel    = torrentChannel
        , _torrentActive     = active
        }

mkTorrentStateSTM :: Torrent -> Integer -> TorrentChannel -> Bool -> STM TorrentTVar
mkTorrentStateSTM torrent left torrentChannel active =
    newTVar $ mkTorrentState torrent left torrentChannel active

mkTorrentDatabase :: TorrentDatabase
mkTorrentDatabase = M.empty

mkTorrentDatabaseSTM :: STM TorrentDatabaseTVar
mkTorrentDatabaseSTM = newTVar mkTorrentDatabase

mkTorrentStatus :: Integer -> TorrentStatus
mkTorrentStatus left =
    TorrentStatus
        { _torrentLeft       = left
        , _torrentIsActive   = True
        , _torrentUploaded   = 0
        , _torrentDownloaded = 0
        , _torrentComplete   = Nothing
        , _torrentIncomplete = Nothing
        , _torrentPeerStatus = if left == 0 then Seeder else Leecher
        }

modify :: TVar a -> (a -> a) -> STM ()
modify tv f = do
    db <- readTVar tv
    writeTVar tv (f db)

torrentMapSTM :: TorrentDatabaseTVar -> (TorrentTVar -> STM a) -> STM [a]
torrentMapSTM tv f = readTVar tv >>= T.mapM f . M.elems

torrentMapSTM_ :: TorrentDatabaseTVar -> (TorrentTVar -> STM ()) -> STM ()
torrentMapSTM_ tv f = readTVar tv >>= T.mapM f >> return ()

addTorrentSTM :: TorrentDatabaseTVar -> TorrentTVar -> STM ()
addTorrentSTM tv torrentTV = do
    torrent <- readTVar torrentTV
    modify tv $ M.insert (torrentInfoHash torrent) torrentTV
  where
    torrentInfoHash = Torrent._torrentInfoHash . _torrent

removeTorrentSTM :: TorrentDatabaseTVar -> InfoHash -> STM ()
removeTorrentSTM tv infoHash = modify tv $ M.delete infoHash

doesTorrentExistSTM :: TorrentDatabaseTVar -> InfoHash -> STM Bool
doesTorrentExistSTM tv infoHash = M.member infoHash `fmap` readTVar tv

getTorrentSTM :: TorrentDatabaseTVar -> InfoHash -> STM (Maybe TorrentTVar)
getTorrentSTM tv infoHash = M.lookup infoHash `fmap` readTVar tv

getInfoHashSTM :: TorrentDatabaseTVar -> STM ([InfoHash])
getInfoHashSTM tv = M.keys `fmap` readTVar tv

getTorrentsSTM :: TorrentDatabaseTVar -> STM [(InfoHash, TorrentState)]
getTorrentsSTM tv = do
    db <- readTVar tv
    mapM extract (M.toList db)
  where
    extract (infoHash, torrentTV) = do
        torrent <- readTVar torrentTV
        return (infoHash, torrent)

getStatisticSTM :: TorrentDatabaseTVar -> STM [(InfoHash, Torrent, TorrentStatus)]
getStatisticSTM tv = do
    db <- readTVar tv
    mapM extractStatus (M.toList db)
  where
    extractStatus (infoHash, torrentTV) = do
        torrent <- readTVar torrentTV
        return (infoHash, _torrent torrent, _torrentStatus torrent)

trackerMessageSTM :: TorrentTVar -> Tracker.TrackerMessage -> STM ()
trackerMessageSTM tv message = do
    torrent <- readTVar tv
    let trackerChan = _torrentTrackerChan . _torrentChannel $ torrent
    writeTChan trackerChan message

adjustStatus :: TorrentTVar -> (TorrentStatus -> TorrentStatus) -> STM ()
adjustStatus tv adjuster = modify tv $ \torrent ->
    torrent { _torrentStatus = adjuster (_torrentStatus torrent) }

getStatusSTM :: TorrentTVar -> STM TorrentStatus
getStatusSTM tv = _torrentStatus `fmap` readTVar tv

getActiveStateSTM :: TorrentTVar -> STM Bool
getActiveStateSTM tv = _torrentActive `fmap` readTVar tv

isTorrentCompleteSTM :: TorrentTVar -> STM Bool
isTorrentCompleteSTM tv = ((== Seeder) . _torrentPeerStatus) `fmap` getStatusSTM tv

transferredUpdateSTM :: TorrentTVar -> Integer -> Integer -> STM ()
transferredUpdateSTM tv upload download = do
    adjustStatus tv $ \torrent -> torrent
        { _torrentUploaded   = _torrentUploaded torrent + upload
        , _torrentDownloaded = _torrentDownloaded torrent + download
        }

pieceCompletedSTM :: TorrentTVar -> Integer -> STM ()
pieceCompletedSTM tv bytes =
    adjustStatus tv $ \rec -> rec { _torrentLeft = _torrentLeft rec - bytes }

torrentCompletedSTM :: TorrentTVar -> STM ()
torrentCompletedSTM tv =
    adjustStatus tv $ \rec -> rec { _torrentPeerStatus = Seeder }

trackerUpdatedSTM :: TorrentTVar -> Maybe Integer -> Maybe Integer -> STM ()
trackerUpdatedSTM tv complete incomplete =
    adjustStatus tv $ \torrent -> torrent
        { _torrentComplete   = complete
        , _torrentIncomplete = incomplete
        }

torrentChangeActiveStateSTM :: TorrentTVar -> Bool -> STM ()
torrentChangeActiveStateSTM tv state = modify tv $ \torrent ->
        torrent { _torrentActive = state }
