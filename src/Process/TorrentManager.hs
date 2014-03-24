{-# LANGUAGE ScopedTypeVariables #-}

module Process.TorrentManager
    ( TorrentManagerMessage(..)
    , runTorrentManager
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

import FS
import Digest (digest)
import Torrent
import Torrent.BCode (BCode)

import Process
import ProcessGroup
import Process.Channel
import Process.Status
import Process.Tracker
import Process.FileAgent
import Process.PeerManager as PeerManager
import Process.PieceManager
import Process.ChokeManager


data TorrentManagerMessage
    = TorrentMAddTorrent FilePath
    | TorrentMRemoveTorrent FilePath
    | TorrentMTerminate
    deriving (Show)


data PConf = PConf
    { _peerId       :: PeerId
    , _statV        :: TVar [UpDownStat]
    , _threadV      :: TVar [(ProcessGroup, MVar ())]
    , _torrentMChan :: TChan TorrentManagerMessage
    , _statusChan   :: TChan StatusMessage
    , _peerMChan    :: TChan PeerManagerMessage
    , _chokeMChan   :: TChan ChokeManagerMessage
    }

instance ProcessName PConf where
    processName _ = "TorrentManager"

type PState = ()


runTorrentManager :: PeerId -> TVar [UpDownStat]
    -> TChan TorrentManagerMessage
    -> TChan StatusMessage
    -> TChan PeerManagerMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runTorrentManager peerId statV torrentMChan statusChan peerMChan chokeMChan = do
    threadV <- newTVarIO []
    let pconf = PConf peerId statV threadV torrentMChan statusChan peerMChan chokeMChan
    catchProcess pconf () process terminate


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState TorrentManagerMessage
wait = do
    torrentMChan <- asks _torrentMChan
    liftIO $ atomically $ readTChan torrentMChan


receive :: TorrentManagerMessage -> Process PConf PState ()
receive message =
    case message of
        TorrentMAddTorrent torrentFile -> do
            debugP $ "Добавление торрента: " ++ torrentFile
            startTorrent torrentFile
        TorrentMRemoveTorrent _torrentFile -> do
            errorP $ "Удаление торрента не реализованно"
            stopProcess
        TorrentMTerminate -> do
            errorP $ "Принудительное завершение"
            stopProcess


terminate :: PConf -> IO ()
terminate pconf = do
    threads <- atomically $ readTVar threadV
    forM_ threads $ \(group, stopM) -> do
        stopGroup group
        takeMVar stopM
  where
    threadV = _threadV pconf


startTorrent :: FilePath -> Process PConf PState ()
startTorrent torrentFile = do
    bcAttempt <- liftIO $ openTorrent torrentFile
    case bcAttempt of
        Right bc -> case mkTorrent digest bc of
            Just torrent -> do
                existsV    <- liftIO newEmptyTMVarIO
                statusChan <- asks _statusChan
                liftIO . atomically $ writeTChan statusChan $
                    ExistsTorrent (_torrentInfoHash torrent) existsV
                exists <- liftIO . atomically $ takeTMVar existsV
                unless exists $ startTorrent' bc torrent
            Nothing ->
                parseFailure
        Left msg ->
            openFailure msg
  where
    openFailure msg = do
        warningP $ "Не удается открыть torrent-файл " ++ torrentFile
        warningP $ msg
    parseFailure = do
        warningP $ "Не удается прочитать torrent-файл " ++ torrentFile


startTorrent' :: BCode -> Torrent -> Process PConf () ()
startTorrent' bc torrent = do
    peerId       <- asks _peerId
    statV        <- asks _statV
    threadV      <- asks _threadV
    peerMChan    <- asks _peerMChan
    statusChan   <- asks _statusChan
    chokeMChan   <- asks _chokeMChan
    torrentMChan <- asks _torrentMChan

    (target, pieceArray, pieceHaveMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft pieceArray pieceHaveMap
        infohash = _torrentInfoHash torrent

    fsChan      <- liftIO newTChanIO
    pieceMChan  <- liftIO newTChanIO
    trackerChan <- liftIO newTChanIO

    liftIO . atomically $ do
       writeTChan trackerChan $ TrackerStart
       writeTChan peerMChan   $ PeerMAddTorrent infohash statV pieceArray pieceMChan fsChan
       writeTChan statusChan  $ StatusAddTorrent infohash left trackerChan

    let allForOne =
            [ runTracker peerId infohash torrent defaultPort trackerChan statusChan peerMChan
            , runFileAgent target pieceArray fsChan
            , runPieceManager infohash pieceArray pieceHaveMap pieceMChan fsChan statusChan chokeMChan
            ]

    stopM <- liftIO newEmptyMVar
    group <- liftIO initGroup
    _     <- liftIO $
                forkFinally
                    (runTorrent group allForOne torrentMChan)
                    (stopTorrent stopM torrentMChan)
    liftIO . atomically $ do
        threads <- readTVar threadV
        writeTVar threadV ((group, stopM) : threads)

    return ()
  where
    runTorrent group allForOne torrentMChan = do
        runGroup group allForOne >> return ()
    stopTorrent stopM torrentMChan _reason = do
        atomically $ writeTChan torrentMChan TorrentMTerminate
        putMVar stopM ()



