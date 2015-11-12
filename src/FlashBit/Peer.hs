{-# LANGUAGE ScopedTypeVariables #-}

module FlashBit.Peer
    ( runPeer
    ) where

import Control.Exception
import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import qualified Data.ByteString as B
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

import Torrent
import Process
import ProcessGroup
import qualified Torrent.Message as TM
import FlashBit.Peer.Main
import FlashBit.Peer.Sender
import FlashBit.Peer.Receiver
import qualified FlashBit.PeerDatabase as PeerDatabase
import qualified FlashBit.TorrentDatabase as TorrentDatabase
import qualified FlashBit.PeerManager.Chan as PeerManager


data PConf = PConf
    { _peerId          :: PeerId
    , _sockaddr        :: S.SockAddr
    , _torrentDatabase :: TorrentDatabase.TorrentDatabaseTVar
    , _peerManagerChan :: TChan PeerManager.PeerEventMessage
    }

instance ProcessName PConf where
    processName pconf = "Peer [" ++ show (_sockaddr pconf) ++ "]"

type PState = ()

data PeerConnection = PeerConnection
    { _peerSocket   :: S.Socket
    , _peerInfoHash :: InfoHash
    , _peerRemain   :: B.ByteString
    , _peerRecevied :: Integer
    , _peerSended   :: Integer
    }

capabilities :: [Capability]
capabilities = []

runPeer :: S.SockAddr -> PeerId -> Either S.Socket InfoHash
        -> TorrentDatabase.TorrentDatabaseTVar
        -> TChan PeerManager.PeerEventMessage
        -> IO ()
runPeer sockaddr peerId sockOrInfoHash torrentDatabase peerManagerChan = do
    let pconf   = PConf peerId sockaddr torrentDatabase peerManagerChan
        pstate  = ()
        process = either accept connect sockOrInfoHash
    wrapProcess pconf pstate process

accept :: S.Socket -> Process PConf PState ()
accept socket' = do
    peerId <- asks _peerId
    result <- liftIO . try $ do
        (socket, _sockaddr) <- S.accept socket'
        (infoHash, remain, received) <- receiveHandshake socket
        sended <- sendHandshake socket infoHash peerId
        return $ PeerConnection socket infoHash remain received sended
    startPeer result

connect :: InfoHash -> Process PConf PState ()
connect infoHash = do
    peerId   <- asks _peerId
    sockaddr <- asks _sockaddr
    result   <- liftIO . try $ do
        socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
        S.connect socket sockaddr
        sended <- sendHandshake socket infoHash peerId
        -- TODO check info_hash from handshake
        (_, remain, received) <- receiveHandshake socket
        return $ PeerConnection socket infoHash remain received sended
    startPeer result

startPeer :: Either SomeException PeerConnection -> Process PConf PState ()
startPeer (Left e) = sendException e
startPeer (Right connection) = do
    torrentDatabase <- asks _torrentDatabase
    mbTorrent       <- liftIO . atomically $
        TorrentDatabase.getTorrentSTM torrentDatabase (_peerInfoHash connection)
    case mbTorrent of
        Just torrent ->
            runPeerGroup torrent connection
        Nothing      ->
            sendException . toException . userError $ "torrent not found"

sendException :: SomeException -> Process PConf PState ()
sendException e = do
    sockaddr        <- asks _sockaddr
    peerManagerChan <- asks _peerManagerChan
    let message = PeerManager.ConnectException sockaddr e
    liftIO . atomically $ writeTChan peerManagerChan message

sendHandshake :: S.Socket -> InfoHash -> PeerId -> IO Integer
sendHandshake socket infoHash peerId  = do
    let handshake = TM.Handshake peerId infoHash capabilities
    let packet    = TM.encodeHandshake handshake
    SB.sendAll socket packet
    return . fromIntegral . B.length $ packet

receiveHandshake :: S.Socket -> IO (InfoHash, B.ByteString, Integer)
receiveHandshake socket = do
    (remain, received, handshake) <- TM.receiveHandshake socket
    let (TM.Handshake _peerId infoHash _capabilities) = handshake
    return (infoHash, remain, received)

runPeerGroup :: TorrentDatabase.TorrentTVar -> PeerConnection
             -> Process PConf PState ()
runPeerGroup torrentTV (PeerConnection socket infoHash remain received sended) = do
    sockaddr        <- asks _sockaddr
    peerManagerChan <- asks _peerManagerChan

    sendChan  <- liftIO newTChanIO
    peerChan  <- liftIO newTChanIO
    sendTV    <- liftIO $ newTVarIO sended
    receiveTV <- liftIO $ newTVarIO received
    torrent   <- liftIO . atomically $ readTVar torrentTV

    let prefix        = show sockaddr
    let channel       = TorrentDatabase._torrentChannel torrent
    let pieceArray    =
            Torrent._torrentPieceArray . TorrentDatabase._torrent $ torrent
    let numPieces     = pieceArraySize pieceArray
    let fileAgentChan = TorrentDatabase._torrentFileAgentChan channel

    peerGroup <- liftIO initGroup
    peerTV    <- liftIO $
        PeerDatabase.mkPeerState peerGroup infoHash numPieces peerChan
    let actions =
            [ runPeerMain prefix sendTV receiveTV peerTV torrentTV sendChan peerChan
            , runPeerSender prefix socket sendTV fileAgentChan sendChan
            , runPeerReceiver prefix socket remain receiveTV peerChan
            ]
    _result <- liftIO $ bracket_
        (connect' sockaddr peerTV peerManagerChan)
        (disconnect' sockaddr peerManagerChan >> S.sClose socket)
        (runGroup peerGroup actions)
    return ()
  where
    connect' sockaddr tv chan = do
        atomically . writeTChan chan $
            PeerManager.Connected infoHash sockaddr tv
    disconnect' sockaddr chan = do
        atomically $ writeTChan chan $
            PeerManager.Disconnected infoHash sockaddr
