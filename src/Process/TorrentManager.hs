{-# LANGUAGE FlexibleInstances #-}

module Process.TorrentManager
    ( runTorrentManager
    , TorrentManagerMessage(..)
    ) where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

import Data.Word (Word16)
import qualified Data.ByteString.Char8 as B8

import FS
import BCode
import Process
import Torrent
import Protocol
import Platform.Process
import Platform.Supervisor hiding (Terminate)

import qualified Process.Status as Status
import qualified Process.Tracker as Tracker


data PConf = PConf
    { cPeerId :: PeerId
--    , cStatusTVar :: TVar [Status.UpDownStat]
    , cStatusChan :: TChan Status.StatusMessage
    , cTorrentChan :: TChan SupervisorMessage
--    , cTorrentManagerChan :: TChan TorrentManagerMessage
    }

instance ProcessName PConf where
    processName _ = "TorrentManager"


data TorrentManagerMessage
    = AddTorrent FilePath
    | RemoveTorrent FilePath
    | Terminate
    deriving (Eq, Show)


runTorrentManager
    :: PeerId
    -> TVar [Status.UpDownStat]
    -> TChan Status.StatusMessage
    -> TChan SupervisorMessage
    -> TChan TorrentManagerMessage
    -> IO Reason
runTorrentManager peerId _statusTV statusChan torrentChan torrentManagerChan =
    process0 "TorrentManager" conf () wait receive
  where
    conf = PConf peerId statusChan torrentChan
    wait = liftIO . atomically $ readTChan torrentManagerChan
    receive msg = handleMessage msg


handleMessage :: TorrentManagerMessage -> Process PConf () ()
handleMessage message =
    case message of
        AddTorrent torrentFile -> do
            debugP $ "Добавление торрента: " ++ torrentFile
            startTorrent torrentFile
        RemoveTorrent _torrentFile -> do
            errorP $ "Удаление торрентов еще не реализованно"
        Terminate ->
            stopProcess Shutdown


startTorrent :: FilePath -> Process PConf () ()
startTorrent torrentFile = do
    bcAttempt <- liftIO . openTorrent $ torrentFile
    case bcAttempt of
        Right bc -> case mkTorrent bc of
            Just torrent -> do
                retvar <- liftIO newEmptyTMVarIO
                statusChan <- asks cStatusChan
                liftIO . atomically $ writeTChan statusChan $
                    Status.TorrentExists (torrentInfoHash torrent) retvar
                exists <- liftIO . atomically $ takeTMVar retvar
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
    peerId <- asks cPeerId
    torrentChan <- asks cTorrentChan
    -- statusTV <- asks cStatusTVar
    statusChan <- asks cStatusChan
    superChan <- liftIO newTChanIO

    (_targets, pieceArray, pieceDoneMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft pieceArray pieceDoneMap
        spec = specSupervisor (runTorrent peerId torrent defaultPort statusChan superChan) superChan
    liftIO . atomically $
        writeTChan torrentChan $
            AddChild (B8.unpack (torrentInfoHash torrent)) spec
    -- start FS
    liftIO . atomically $
        writeTChan statusChan $
            Status.InsertTorrent (torrentInfoHash torrent) left
    -- liftIO . atomically $ writeTChan trackerChan Start
    return ()


runTorrent :: [Char]
           -> Torrent
           -> Word16
           -> TChan Status.StatusMessage
           -> TChan SupervisorMessage
           -> IO Reason
runTorrent peerId torrent port' statusChan superChan = do
    let specs = do
            trackerChan <- newTChanIO
            atomically $ writeTChan trackerChan Tracker.Start
            return $
                [ ("tracker", specServer2 trackerChan Tracker.Terminate $
                     Tracker.runTracker peerId torrent port' statusChan trackerChan)
                -- FS
                -- PieceManager
                ]
    supervisor0 "Torrent" superChan specs


