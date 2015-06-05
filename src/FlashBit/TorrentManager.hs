{-# LANGUAGE ScopedTypeVariables #-}

module FlashBit.TorrentManager
    ( runTorrentManager
    ) where

import Control.Monad (when, forM_)
import Control.Monad.Reader (liftIO, asks)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Word

import Process
import ProcessGroup
import Torrent
import Torrent.File
import FlashBit.TorrentDatabase
import FlashBit.TorrentManager.Chan
import FlashBit.TorrentThreadDatabase
import FlashBit.PeerDatabase (PeerDatabaseTVar)
import qualified FlashBit.Tracker as Tracker
import qualified FlashBit.FileAgent as FileAgent
import qualified FlashBit.PieceManager as PieceManager
import FlashBit.PeerManager.Chan
import qualified FlashBit.ChokeManager as ChokeManager


data PConf = PConf
    { _peerId             :: PeerId
    , _peerDb             :: PeerDatabaseTVar
    , _threadDb           :: TorrentThreadDatabaseTVar
    , _torrentDb          :: TorrentDatabaseTVar
    , _peerManagerChan    :: TChan PeerManagerMessage
    , _torrentManagerChan :: TChan TorrentManagerMessage
    }

instance ProcessName PConf where
    processName _ = "TorrentManager"

type PState = ()

defaultPort :: Word16
defaultPort = 1369


runTorrentManager :: PeerId -> PeerDatabaseTVar
                  -> TorrentDatabaseTVar
                  -> TChan PeerManagerMessage
                  -> TChan TorrentManagerMessage
                  -> IO ()
runTorrentManager peerId peerDb torrentDb peerMChan torrentMChan = do
    threadDb <- atomically mkTorrentThreadDatabaseSTM
    let pconf  = PConf peerId peerDb threadDb torrentDb peerMChan torrentMChan
    let pstate = ()
    catchProcess pconf pstate process terminate

terminate :: PConf -> IO ()
terminate pconf = do
    threads <- atomically $ getThreadsSTM threadDb
    forM_ threads $ \(_infoHash, (group, stopM)) ->
        stopGroup group >> takeMVar stopM
  where
    threadDb = _threadDb pconf

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState TorrentManagerMessage
wait = do
    torrentManagerChan <- asks _torrentManagerChan
    liftIO . atomically $ readTChan torrentManagerChan

receive :: TorrentManagerMessage -> Process PConf PState ()
receive message =
    case message of
        AddTorrent torrent target startImmediately -> do
            noticeP $ "Add torrent: " ++ show (_torrentName torrent)
            addTorrent torrent target startImmediately

        StopTorrent infoHash -> do
            noticeP  $ "Stop torrent: " ++ showInfoHash infoHash
            stopTorrent infoHash

        StartTorrent infoHash -> do
            noticeP  $ "Start torrent: " ++ showInfoHash infoHash
            startTorrent infoHash

        RemoveTorrent infoHash -> do
            noticeP  $ "Remove torrent: " ++ showInfoHash infoHash
            removeTorrent infoHash


addTorrent :: Torrent -> (FileRec, PieceHaveMap) -> Bool
           -> Process PConf PState ()
addTorrent torrent (target, pieceHaveMap) startImmediately = do
    peerId          <- asks _peerId
    peerDb          <- asks _peerDb
    peerManagerChan <- asks _peerManagerChan

    trackerChan        <- liftIO newTChanIO
    fileAgentChan      <- liftIO newTChanIO
    pieceManagerChan   <- liftIO newTChanIO
    chokeManagerChan   <- liftIO newTChanIO
    pieceBroadcastChan <- liftIO newBroadcastTChanIO

    let infoHash   = Torrent._torrentInfoHash torrent
    let pieceArray = Torrent._torrentPieceArray torrent
    let left       = torrentBytesLeft pieceArray pieceHaveMap
    let channel    = TorrentChannel
            trackerChan
            fileAgentChan
            pieceManagerChan
            pieceBroadcastChan
    torrentTV <- liftIO . atomically $
        mkTorrentStateSTM torrent left channel startImmediately
    noticeP $ "Осталось скачать " ++ show left ++ " байт"

    when startImmediately $ do
        liftIO . atomically $ writeTChan trackerChan Tracker.Start

    let actions =
            [ Tracker.runTracker
                peerId
                torrent
                defaultPort
                torrentTV
                peerManagerChan
                trackerChan
            , FileAgent.runFileAgent
                target
                infoHash
                pieceArray
                fileAgentChan
            , PieceManager.runPieceManager
                infoHash
                pieceArray
                pieceHaveMap
                torrentTV
                fileAgentChan
                pieceBroadcastChan
                pieceManagerChan
            , ChokeManager.runChokeManager
                infoHash
                peerDb
                torrentTV
                chokeManagerChan
            ]
    runTorrentGroup actions infoHash torrentTV trackerChan

runTorrentGroup :: [IO ()] -> InfoHash -> TorrentTVar
                  -> TChan Tracker.TrackerMessage
                  -> Process PConf PState ()
runTorrentGroup actions infoHash torrentTV trackerChan = do
    threadDb  <- asks _threadDb
    torrentDb <- asks _torrentDb

    group <- liftIO initGroup
    stopM <- liftIO newEmptyMVar
    _     <- liftIO $ forkFinally
        (runTorrentG stopM threadDb torrentDb group)
        (stopTorrentG stopM threadDb torrentDb)
    return ()
  where
    runTorrentG stopM threadDb torrentDb group = do
        let thread = (group, stopM)
        atomically $ do
            addTorrentSTM torrentDb torrentTV
            addTorrentThreadSTM threadDb infoHash thread trackerChan
        runGroup group actions >> return ()

    stopTorrentG stopM threadDb torrentDb _reason = do
        atomically $ do
            removeTorrentSTM torrentDb infoHash
            removeTorrentThreadSTM threadDb infoHash
        putMVar stopM ()


removeTorrent :: InfoHash -> Process PConf PState ()
removeTorrent infoHash = do
    threadDb  <- asks _threadDb
    torrentDb <- asks _torrentDb
    stopTorrent infoHash
    liftIO . atomically $ do
        mbTorrentTV <- getTorrentSTM torrentDb infoHash
        case mbTorrentTV of
            Just torrentTV -> do
                active <- getActiveStateSTM torrentTV
                when active retry
            Nothing -> return ()
    torrentThread <- liftIO . atomically $ getTorrentThreadSTM threadDb infoHash
    case torrentThread of
        Just (group, stopM) -> do
            liftIO $ stopGroup group >> takeMVar stopM
        Nothing -> return ()

withTorrent :: InfoHash
            -> (TorrentTVar -> Process PConf PState ())
            -> Process PConf PState ()
withTorrent infoHash action = do
    torrentDb   <- asks _torrentDb
    mbTorrentTV <- liftIO . atomically $ getTorrentSTM torrentDb infoHash
    case mbTorrentTV of
        Just torrentTV -> action torrentTV
        Nothing        -> return ()

stopTorrent :: InfoHash -> Process PConf PState ()
stopTorrent infoHash = withTorrent infoHash $ \torrentTV -> do
    peerManagerChan <- asks _peerManagerChan
    liftIO . atomically $ do
        trackerMessageSTM torrentTV Tracker.Stop
        writeTChan peerManagerChan $ StopTorrentPeers infoHash

startTorrent :: InfoHash -> Process PConf PState ()
startTorrent infoHash = withTorrent infoHash $ \torrentTV ->
    liftIO . atomically $ trackerMessageSTM torrentTV Tracker.Start
