{-# LANGUAGE ScopedTypeVariables #-}

module FlashBit.Tracker
    ( runTracker
    , TrackerMessage(..)
    ) where

import Data.Word
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Exception

import Process
import Torrent
import Torrent.Announce
import FlashBit.Tracker.Chan
import FlashBit.Tracker.State
import FlashBit.TorrentDatabase (TorrentTVar)
import qualified FlashBit.TorrentDatabase as TorrentDatabase
import FlashBit.PeerManager.Chan (PeerManagerMessage)
import qualified FlashBit.PeerManager.Chan as PeerManager


data PConf = PConf
    { _peerId          :: PeerId
    , _infoHash        :: InfoHash
    , _localPort       :: Word16
    , _torrentTV       :: TorrentTVar
    , _trackerChan     :: TChan TrackerMessage
    , _peerManagerChan :: TChan PeerManagerMessage
    }

instance ProcessName PConf where
    processName pconf = "Tracker [" ++ showInfoHash (_infoHash pconf) ++ "]"

type PState = TrackerState

defaultTimerInterval :: Integer
defaultTimerInterval = 15 * 60

runTracker :: PeerId -> Torrent -> Word16 -> TorrentTVar
           -> TChan PeerManagerMessage
           -> TChan TrackerMessage
           -> IO ()
runTracker peerId torrent localPort torrentTV peerManagerChan trackerChan = do
    let infoHash = _torrentInfoHash torrent
    let announce = _torrentAnnounceList torrent
    let pconf    = PConf
            { _peerId          = peerId
            , _infoHash        = infoHash
            , _localPort       = localPort
            , _torrentTV       = torrentTV
            , _trackerChan     = trackerChan
            , _peerManagerChan = peerManagerChan
            }
    let pstate = mkTrackerState announce
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
receive message =
    case message of
        Stop -> do
            noticeP "Stop"
            trackerStop >> talkTracker

        Start -> do
            noticeP "Start"
            trackerStart >> talkTracker

        Complete -> do
            noticeP "Complete"
            trackerComplete >> talkTracker

        Tick x -> do
            validTick <- trackerCheckTick x
            when validTick (noticeP "Tick" >> talkTracker)

talkTracker :: Process PConf PState ()
talkTracker = getTorrentStatus >>= pokeTracker >>= timerUpdate

pokeTracker :: TorrentStatus -> Process PConf PState (Integer, Maybe Integer)
pokeTracker torrentStatus = do
    announceList   <- S.gets _announceList
    trackerParams  <- buildTrackerParams torrentStatus
    trackerAttempt <- liftIO . try $ askTracker trackerParams announceList
    case trackerAttempt of
        Left (err :: TrackerResponseError) -> do
            errorP $ show err
            return (defaultTimerInterval, Nothing)
        Right (announceList', response) -> do
            trackerUpdateStats response
            trackerUpdatePeers response
            trackerUpdateAnnounce announceList'
            trackerEventTransition
            return (_trackerInterval response, _trackerMinInterval response)

getTorrentStatus :: Process PConf PState TorrentStatus
getTorrentStatus = do
    torrentTV <- R.asks _torrentTV
    liftIO . atomically $ TorrentDatabase.getStatusSTM torrentTV

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
    nextTick     <- trackerUpdateTimer
    trackerChan  <- R.asks _trackerChan
    let timeout' = fromIntegral timeout
    let emitTick = atomically $ writeTChan trackerChan (Tick nextTick)
    _ <- liftIO $ setTimeout timeout' emitTick
    noticeP $ "Next announce will be made after " ++ show timeout ++ " seconds"

trackerUpdateStats :: TrackerResponse -> Process PConf PState ()
trackerUpdateStats response = do
    activity       <- (Stopped /=) `R.liftM` getTrackerStatus
    torrentTV      <- R.asks _torrentTV
    let complete   = _trackerComplete response
    let incomplete = _trackerIncomplete response
    liftIO . atomically $ do
        TorrentDatabase.trackerUpdatedSTM torrentTV complete incomplete
        TorrentDatabase.torrentChangeActiveStateSTM torrentTV activity

trackerUpdatePeers :: TrackerResponse -> Process PConf PState ()
trackerUpdatePeers response = do
    infoHash        <- R.asks _infoHash
    peerManagerChan <- R.asks _peerManagerChan
    let peers       = _trackerPeers response
    let message     = PeerManager.NewTrackerPeers infoHash peers
    liftIO . atomically $ writeTChan peerManagerChan message
