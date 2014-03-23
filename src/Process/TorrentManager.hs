module Process.TorrentManager
    ( TorrentManagerMessage(..)
    , runTorrentManager
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

import FS
import Digest (digest)
import Torrent
import Torrent.BCode (BCode)
import Process
import Process.Status
import Process.PeerManager
import Process.ChokeManager


data TorrentManagerMessage
    = AddTorrent FilePath
    | RemoveTorrent FilePath
    deriving (Show)


data PConf = PConf
    { _peerId       :: PeerId
    , _statV        :: TVar [UpDownStat]
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
    let pconf = PConf peerId statV torrentMChan statusChan peerMChan chokeMChan
    wrapProcess pconf () process


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
        Process.TorrentManager.AddTorrent torrentFile -> do
            debugP $ "Добавление торрента: " ++ torrentFile
            startTorrent torrentFile
        Process.TorrentManager.RemoveTorrent _torrentFile -> do
            errorP $ "Удаление торрента не реализованно"
            stopProcess


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
                unless (exists) $ startTorrent' bc torrent
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
    peerMChan    <- asks _peerMChan
    statusChan   <- asks _statusChan
    chokeMChan   <- asks _chokeMChan
    torrentMChan <- asks _torrentMChan

    (target, pieceArray, pieceHaveMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft pieceArray pieceHaveMap
        infoHash = _torrentInfoHash torrent

    -- pieceDB <- PieceManager.createPieceDB pieceHaveMap pieceArray
    fsChan      <- liftIO newTChanIO
    pieceMChan  <- liftIO newTChanIO
    trackerChan <- liftIO newTChanIO
    let allForOne = []
            -- [ runFileAgent target pieceArray fsChan
            -- , runPieceManager pieceDB infoHash pieceMChan fsChan statusChan chokeChan
            -- , runTracker peerId infoHash torrent defaultPort trackerChan statusChan peerMChan
            -- ]
    -- liftIO . atomically $ do
    --    writeTChan trackerChan $ Tracker.Start
    --    writeTChan statusChan  $ Status.AddTorrent infoHash left trackerChan
    --    writeTChan peerMChan   $ PeerManager.AddTorrent infoHash statV pieceMChan fsChan pieceArray

    return ()


