{-# LANGUAGE ScopedTypeVariables #-}

module FlashBit.TorrentManager
    ( runTorrentManager
    , TorrentManagerMessage(..)
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (unless, forM_)
import Control.Monad.Reader (liftIO, asks)

import Process
import ProcessGroup
import Torrent
import Torrent.File
import FlashBit.TorrentDatabase
import FlashBit.TorrentThreadDatabase
import FlashBit.PeerDatabase (PeerDatabaseTVar)
import qualified FlashBit.Tracker as Tracker
import qualified FlashBit.FileAgent as FileAgent
import qualified FlashBit.PieceManager as PieceManager
import qualified FlashBit.PeerManager.Chan as PeerManager
import qualified FlashBit.ChokeManager as ChokeManager


data PConf = PConf
    { _peerId           :: PeerId
    , _peerDb           :: PeerDatabaseTVar
    , _threadDb         :: TorrentThreadDatabaseTVar
    , _torrentDb        :: TorrentDatabaseTVar
    , _peerManagerChan  :: TChan PeerManager.PeerManagerMessage
    , _torrentChan      :: TChan TorrentManagerMessage
    }

instance ProcessName PConf where
    processName _ = "TorrentManager"

type PState = ()

data TorrentManagerMessage
    = AddTorrent FilePath
    | RemoveTorrent FilePath
    | Shutdown (MVar ())


runTorrentManager :: PeerId
                  -> PeerDatabaseTVar
                  -> TorrentDatabaseTVar
                  -> TChan PeerManager.PeerManagerMessage
                  -> TChan TorrentManagerMessage
                  -> IO ()
runTorrentManager peerId peerDb torrentDb peerManagerChan torrentChan = do
    threadDb <- atomically mkTorrentThreadDatabaseSTM
    let pconf  = PConf peerId peerDb threadDb torrentDb peerManagerChan torrentChan
        pstate = ()
    catchProcess pconf pstate process terminate

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState TorrentManagerMessage
wait = do
    torrentChan <- asks _torrentChan
    liftIO . atomically $ readTChan torrentChan

receive :: TorrentManagerMessage -> Process PConf PState ()
receive message =
    case message of
        AddTorrent torrentFile -> do
            debugP $ "Добавляем торрент: " ++ torrentFile
            addTorrent torrentFile

        RemoveTorrent torrentFile -> do
            debugP $ "Удаленям торрент: " ++ torrentFile
            errorP $ "Удаление торрента не реализованно"
            stopProcess

        Shutdown waitV -> do
            debugP $ "Завершение (shutdown)"
            shutdown waitV

terminate :: PConf -> IO ()
terminate pconf = do
    let threadDb = _threadDb pconf
    threads  <- atomically $ getAllTorrentThreadsSTM threadDb
    forM_ threads $ \(_, (group, stopM)) -> stopGroup group >> takeMVar stopM
    return ()

shutdown :: MVar () -> Process PConf PState ()
shutdown waitV = do
    threadDb    <- asks _threadDb
    threads     <- liftIO . atomically $ getAllTorrentThreadsSTM threadDb
    waitTracker <- liftIO newEmptyMVar
    forM_ threads $ \(infoHash, _) -> do
        let message = Tracker.Shutdown waitTracker
        mbTrackerChan <- liftIO . atomically $ getTorrentTrackerChanSTM threadDb infoHash
        case mbTrackerChan of
            Just trackerChan -> do
                liftIO . atomically $ writeTChan trackerChan message
                liftIO $ takeMVar waitTracker
            _                -> return ()
        return ();
    liftIO $ putMVar waitV ()


-- TODO: rewrite
addTorrent :: FilePath -> Process PConf PState ()
addTorrent torrentFile = do
    torrentDb <- asks _torrentDb
    bcAttempt <- liftIO . try $ openTorrent torrentFile
    case bcAttempt of
        Right bc -> case mkTorrentMeta bc of
            Just torrentMeta -> do
                let infoHash = _torrentMetaInfoHash torrentMeta
                exist <- liftIO . atomically $ doesTorrentExistSTM torrentDb infoHash
                unless exist $ do
                    openAttempt <- liftIO . try $ openTarget "." bc
                    case openAttempt of
                        Right (target, pieceArray, pieceHaveMap) -> do
                            startTorrent target pieceArray pieceHaveMap torrentMeta
                        Left (e :: SomeException) -> openFailure e
            Nothing -> parseFailure
        Left (e :: SomeException) -> openFailure e
  where
    openFailure msg = do
        warningP $ "Не удается открыть torrent-файл " ++ torrentFile
        debugP   $ show msg
    parseFailure =
        warningP $ "Не удается прочитать torrent-файл " ++ torrentFile


startTorrent :: FileRec -> PieceArray -> PieceHaveMap -> TorrentMeta -> Process PConf PState ()
startTorrent target pieceArray pieceHaveMap torrentMeta = do
    peerId             <- asks _peerId
    peerDb             <- asks _peerDb
    peerManagerChan    <- asks _peerManagerChan
    trackerChan        <- liftIO newTChanIO
    fileAgentChan      <- liftIO newTChanIO
    pieceManagerChan   <- liftIO newTChanIO
    chokeManagerChan   <- liftIO newTChanIO
    pieceBroadcastChan <- liftIO newBroadcastTChanIO

    let left     = bytesLeft pieceArray pieceHaveMap
    let infoHash = _torrentMetaInfoHash torrentMeta
    let channel  = TorrentChannel
            trackerChan
            fileAgentChan
            pieceManagerChan
            pieceBroadcastChan
    torrentTV <- liftIO . atomically $ mkTorrentStateSTM infoHash pieceArray left channel
    debugP $ "Осталось скачать " ++ show left ++ " байт"

    -- TODO optional auto-start
    liftIO . atomically $ writeTChan trackerChan Tracker.Start

    let actions =
            [ Tracker.runTracker
                peerId
                torrentMeta
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
                peerDb
                torrentTV
                chokeManagerChan
            ]
    startTorrentGroup actions infoHash torrentTV trackerChan

startTorrentGroup :: [IO ()] -> InfoHash -> TorrentTVar
                  -> TChan Tracker.TrackerMessage
                  -> Process PConf PState ()
startTorrentGroup actions infoHash torrentTV trackerChan = do
    threadDb  <- asks _threadDb
    torrentDb <- asks _torrentDb
    group     <- liftIO initGroup
    stopM     <- liftIO newEmptyMVar
    _         <- liftIO $ forkFinally
        (runTorrent stopM threadDb torrentDb group)
        (stopTorrent stopM threadDb torrentDb)
    return ()
  where
    runTorrent stopM threadDb torrentDb group = do
        atomically $ do
            addTorrentSTM torrentDb torrentTV
            addTorrentThreadSTM threadDb infoHash (group, stopM) trackerChan
        runGroup group actions >> return ()

    stopTorrent stopM threadDb torrentDb _reason = do
        atomically $ do
            removeTorrentSTM torrentDb infoHash
            removeTorrentThreadSTM threadDb infoHash
        putMVar stopM ()
