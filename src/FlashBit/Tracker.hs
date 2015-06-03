module FlashBit.Tracker
    ( runTracker
    , TrackerMessage(..)
    ) where

import Data.Word
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Timer

import Process
import Torrent
import Torrent.Announce
import FlashBit.Tracker.Chan
import FlashBit.Tracker.State
import FlashBit.TorrentDatabase (TorrentTVar)
import qualified FlashBit.TorrentDatabase as TorrentDatabase
import qualified FlashBit.PeerManager.Chan as PeerManager


data PConf = PConf
    { _peerId          :: PeerId
    , _infoHash        :: InfoHash
    , _localPort       :: Word16
    , _torrentTV       :: TorrentTVar
    , _peerManagerChan :: TChan PeerManager.PeerManagerMessage
    , _trackerChan     :: TChan TrackerMessage
    }

instance ProcessName PConf where
    processName pconf = "Tracker [" ++ showInfoHash (_infoHash pconf) ++ "]"

type PState = TrackerState


runTracker :: PeerId -> TorrentMeta -> Word16 -> TorrentTVar
    -> TChan PeerManager.PeerManagerMessage
    -> TChan TrackerMessage
    -> IO ()
runTracker peerId torrentMeta port torrentTV peerManagerChan trackerChan = do
    let infoHash     = _torrentMetaInfoHash torrentMeta
        announceList = _torrentMetaAnnounceList torrentMeta
    let pconf  = PConf peerId infoHash port torrentTV peerManagerChan trackerChan
    let pstate = mkTrackerState announceList
    wrapProcess pconf pstate process

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState TrackerMessage
wait = do
    trackerChan <- R.asks _trackerChan
    liftIO . atomically $ readTChan trackerChan

receive :: TrackerMessage -> Process PConf PState ()
receive message = do
    case message of
        Stop -> do
            noticeP "Прекращаем скачивать"
            trackerStop >> talkTracker

        Start -> do
            noticeP "Начинаем скачивать"
            trackerStart >> talkTracker

        Complete -> do
            noticeP "Торрент полностью скачен"
            trackerComplete >> talkTracker

        Shutdown waitV -> do
            noticeP "Завершение (shutdown)"
            trackerStop >> talkTracker
            liftIO $ putMVar waitV ()

        Tick x -> do
            validTick <- trackerCheckTick x
            when validTick talkTracker

talkTracker :: Process PConf PState ()
talkTracker = getTorrentStatus >>= pokeTracker >>= timerUpdate

getTorrentStatus :: Process PConf PState TorrentStatus
getTorrentStatus = do
    torrentTV <- R.asks _torrentTV
    liftIO . atomically $ TorrentDatabase.getStatusSTM torrentTV

pokeTracker :: TorrentStatus -> Process PConf PState (Integer, Maybe Integer)
pokeTracker torrentStatus = do
    infoHash        <- R.asks _infoHash
    peerManagerChan <- R.asks _peerManagerChan
    announceList    <- S.gets _announceList
    params          <- buildTrackerParams torrentStatus

    -- TODO `Control.Exception.try`
    (announceList', response) <- liftIO $ askTracker params announceList
    trackerUpdateAnnounce announceList'
    trackerUpdateStats response
    trackerEventTransition

    let newPeers = PeerManager.NewTrackerPeers infoHash (_trackerPeers response)
    liftIO . atomically $ writeTChan peerManagerChan newPeers

    return (_trackerInterval response, _trackerMinInterval response)

buildTrackerParams :: TorrentStatus -> Process PConf PState TrackerParam
buildTrackerParams torrentStatus = do
    peerId        <- R.asks _peerId
    infoHash      <- R.asks _infoHash
    localPort     <- R.asks _localPort
    trackerStatus <- S.gets _trackerStatus
    return $ TrackerParam
        { _paramPeerId     = peerId
        , _paramInfoHash   = infoHash
        , _paramLocalPort  = localPort
        , _paramStatus     = trackerStatus
        , _paramLeft       = _torrentLeft torrentStatus
        , _paramUploaded   = _torrentUploaded torrentStatus
        , _paramDownloaded = _torrentDownloaded torrentStatus
        }

timerUpdate :: (Integer, Maybe Integer) -> Process PConf PState ()
timerUpdate (timeout, _minTimeout) = do
    trackerChan  <- R.asks _trackerChan
    nextTick     <- trackerUpdateTimer
    let timeout' = fromIntegral timeout
    let emitTick = atomically $ writeTChan trackerChan (Tick nextTick)
    _ <- liftIO $ setTimeout timeout' emitTick
    noticeP $ "Следующие обращение к трекеру через " ++ show timeout

trackerUpdateStats :: TrackerResponse -> Process PConf PState ()
trackerUpdateStats response = do
    torrentTV      <- R.asks _torrentTV
    let complete   = _trackerComplete response
    let incomplete = _trackerIncomplete response
    liftIO . atomically $
        TorrentDatabase.trackerUpdatedSTM torrentTV complete incomplete
