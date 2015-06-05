module FlashBit.API
    ( API
    , Config(..)
    , spawn
    , shutdown
    , addTorrent
    , removeTorrent
    , stopTorrent
    , startTorrent
    , getTorrents
    , getStatistic
    , module FlashBit.Config
    ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import System.Random

import Torrent
import Torrent.File
import ProcessGroup

import FlashBit.Config
import FlashBit.Listen as Listen
import FlashBit.PeerManager as PeerManager
import FlashBit.PeerManager.Chan as PeerManager
import FlashBit.TorrentManager as TorrentManager
import FlashBit.TorrentManager.Chan as TorrentManager
import FlashBit.PeerDatabase as PeerDatabase
import FlashBit.TorrentDatabase as TorrentDatabase
import qualified FlashBit.TorrentDatabase as TD


data API = API
    { _config             :: Config
    , _peerId             :: PeerId
    , _peerDatabase       :: PeerDatabaseTVar
    , _torrentDatabase    :: TorrentDatabaseTVar
    , _peerManagerChan    :: TChan PeerManagerMessage
    , _torrentManagerChan :: TChan TorrentManagerMessage
    }

protoVersion :: String
protoVersion = "0010"

initialize :: Config -> IO API
initialize config = do
    stdGen <- newStdGen
    let peerId = mkPeerId stdGen protoVersion

    peerDatabase       <- atomically mkPeerDatabaseSTM
    torrentDatabase    <- atomically mkTorrentDatabaseSTM
    peerManagerChan    <- newTChanIO
    torrentManagerChan <- newTChanIO

    return $ API
        config
        peerId
        peerDatabase
        torrentDatabase
        peerManagerChan
        torrentManagerChan

start :: API -> IO (Either SomeException ())
start api = do
    let peerId             = _peerId api
    let peerDatabase       = _peerDatabase api
    let torrentDatabase    = _torrentDatabase api
    let peerManagerChan    = _peerManagerChan api
    let torrentManagerChan = _torrentManagerChan api
    let localPort          = _localPort . _config $ api

    group <- initGroup
    let actions =
            [ runListen localPort peerManagerChan
            , runPeerManager
                peerId
                peerDatabase
                torrentDatabase -- used for searching info_hash after handshake
                peerManagerChan
            , runTorrentManager
                peerId          -- used by a tracker
                peerDatabase    -- used by a choke manager
                torrentDatabase
                peerManagerChan -- used for sending peers from tracker
                torrentManagerChan
            ]
    runGroup group actions

spawn :: Config -> (Either SomeException () -> IO ()) -> IO API
spawn config andThen = do
    api <- initialize config
    _threadId <- forkFinally (start api) sequel
    return api
  where
    sequel (Left e) = andThen (Left e)
    sequel (Right result) = andThen result

shutdown :: API -> IO ()
shutdown api = do
    torrents <- atomically $ TD.getInfoHashSTM torrentDb
    mapM_ (stopTorrent api) torrents
    atomically $ do
        states <- TD.torrentMapSTM torrentDb TD.getActiveStateSTM
        all (False ==) states `unless` retry
  where
    torrentDb = _torrentDatabase api

addTorrent :: API -> FilePath -> Bool -> IO ()
addTorrent api torrentFile startImmediately = do
    torrent <- openTorrent torrentFile
    let infoHash     = _torrentInfoHash torrent
    let downloadPath = _downloadPath . _config $ api
    exist <- atomically $ doesTorrentExistSTM torrentDatabase infoHash
    unless exist $ do
        target <- openTarget torrent downloadPath
        atomically . writeTChan torrentManagerChan $
            TorrentManager.AddTorrent torrent target startImmediately
  where
    torrentDatabase    = _torrentDatabase api
    torrentManagerChan = _torrentManagerChan api

removeTorrent :: API -> InfoHash -> IO ()
removeTorrent api infoHash = atomically $
    writeTChan torrentManagerChan $ RemoveTorrent infoHash
  where
    torrentManagerChan = _torrentManagerChan api

stopTorrent :: API -> InfoHash -> IO ()
stopTorrent api infoHash = atomically $
    writeTChan torrentManagerChan $ StopTorrent infoHash
  where
    torrentManagerChan = _torrentManagerChan api

startTorrent :: API -> InfoHash -> IO ()
startTorrent api infoHash = atomically $
    writeTChan torrentManagerChan $ StartTorrent infoHash
  where
    torrentManagerChan = _torrentManagerChan api

getTorrents :: API -> IO [(InfoHash, TorrentState)]
getTorrents api = do
    let torrentDatabase = _torrentDatabase api
    atomically $ TorrentDatabase.getTorrentsSTM torrentDatabase

getStatistic :: API -> IO [(InfoHash, Torrent, TorrentStatus)]
getStatistic api = do
    let torrentDatabase = _torrentDatabase api
    atomically $ TorrentDatabase.getStatisticSTM torrentDatabase
