{-# LANGUAGE ScopedTypeVariables #-}

module FlashBit.Peer
    ( runPeer
    ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader (liftIO, asks)
import qualified Data.ByteString as B
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

import Process
import ProcessGroup
import Torrent
import qualified Torrent.Message as TM
import FlashBit.Peer.Main
import FlashBit.Peer.Sender
import FlashBit.Peer.Receiver
import FlashBit.PeerDatabase
import qualified FlashBit.PeerManager.Chan as PeerManager
import qualified FlashBit.TorrentDatabase as TorrentDatabase


data PConf = PConf
    { _peerId        :: PeerId
    , _sockaddr      :: S.SockAddr
    , _torrentDb     :: TorrentDatabase.TorrentDatabaseTVar
    , _peerEventChan :: TChan PeerManager.PeerEventMessage
    }

instance ProcessName PConf where
    processName pconf = "Peer [" ++ show (_sockaddr pconf) ++ "]"

type PState = ()

capabilities :: [Capability]
capabilities = []

runPeer :: S.SockAddr -> PeerId -> Either S.Socket InfoHash
        -> TorrentDatabase.TorrentDatabaseTVar
        -> TChan PeerManager.PeerEventMessage
        -> IO ()
runPeer sockaddr peerId sockOrInfoHash torrentDb peerEventChan = do
    let pconf   = PConf peerId sockaddr torrentDb peerEventChan
        process = either accept connect sockOrInfoHash
    wrapProcess pconf () process

accept :: S.Socket -> Process PConf PState ()
accept socket' = do
    peerId <- asks _peerId
    result <- liftIO . try $ do
        (socket, _sockaddr) <- S.accept socket'
        (infoHash, remain, consumed) <- receiveHandshake socket
        sended <- sendHandshake socket infoHash peerId
        return (socket, infoHash, remain, consumed, sended)
    startPeer result

connect :: InfoHash -> Process PConf PState ()
connect infoHash = do
    peerId   <- asks _peerId
    sockaddr <- asks _sockaddr
    result   <- liftIO . try $ do
        socket <- S.socket S.AF_INET S.Stream S.defaultProtocol
        S.connect socket sockaddr
        sended <- sendHandshake socket infoHash peerId
        (_, remain, consumed) <- receiveHandshake socket
        return (socket, infoHash, remain, consumed, sended)
    startPeer result

startPeer :: Either SomeException (S.Socket, InfoHash, B.ByteString, Integer, Integer)
          -> Process PConf PState ()
startPeer (Left e) = sendException e
startPeer (Right (socket, infoHash, remain, consumed, sended)) = do
    torrentDb <- asks _torrentDb
    mbTorrent <- liftIO . atomically $ TorrentDatabase.getTorrentSTM torrentDb infoHash
    case mbTorrent of
        Just torrent ->
            runPeerGroup socket infoHash torrent remain consumed sended
        Nothing      ->
            sendException $ toException (userError "torrent not found")

sendException :: SomeException -> Process PConf PState ()
sendException e = do
    sockaddr      <- asks _sockaddr
    peerEventChan <- asks _peerEventChan
    let message = PeerManager.ConnectException sockaddr e
    liftIO . atomically $ writeTChan peerEventChan message

sendHandshake :: S.Socket -> InfoHash -> PeerId -> IO Integer
sendHandshake socket infoHash peerId  = do
    let handshake = TM.Handshake peerId infoHash capabilities
    let packet    = TM.encodeHandshake handshake
    SB.sendAll socket packet
    return . fromIntegral . B.length $ packet

receiveHandshake :: S.Socket -> IO (InfoHash, B.ByteString, Integer)
receiveHandshake socket = do
    (remain, consumed, handshake) <- TM.receiveHandshake socket
    let (TM.Handshake _peerId infoHash _capabilities) = handshake
    return (infoHash, remain, consumed)

runPeerGroup :: S.Socket -> InfoHash -> TorrentDatabase.TorrentTVar -> B.ByteString
             -> Integer
             -> Integer
             -> Process PConf PState ()
runPeerGroup socket infoHash torrentTV remain received sended = do
    sockaddr      <- asks _sockaddr
    peerEventChan <- asks _peerEventChan

    sendChan  <- liftIO newTChanIO
    peerChan  <- liftIO newTChanIO
    sendTV    <- liftIO $ newTVarIO sended
    receiveTV <- liftIO $ newTVarIO received
    torrent   <- liftIO . atomically $ readTVar torrentTV

    let prefix             = show sockaddr
    let channel            = TorrentDatabase._torrentChannel torrent
    let pieceArray         = TorrentDatabase._torrentPieceArray torrent
    let fileAgentChan      = TorrentDatabase._torrentFileAgentChan channel
    let pieceManagerChan   = TorrentDatabase._torrentPieceManagerChan channel
    let pieceBroadcastChan = TorrentDatabase._torrentPieceBroadcastChan channel
    pieceBroadcastChanDup  <- liftIO . atomically $ dupTChan pieceBroadcastChan
    peerState              <- liftIO $ mkPeerState infoHash (pieceArraySize pieceArray) peerChan

    group <- liftIO initGroup
    let actions =
            [ runPeerMain prefix infoHash pieceArray sendTV receiveTV
                peerState
                sendChan
                peerChan
                torrentTV
                pieceManagerChan
                pieceBroadcastChanDup
            , runPeerSender prefix socket sendTV fileAgentChan sendChan
            , runPeerReceiver prefix socket remain receiveTV peerChan
            ]
    result <- liftIO $ bracket_
        (connect' sockaddr peerState peerEventChan)
        (disconnect' sockaddr peerEventChan >> S.sClose socket)
        (runGroup group actions)
    case result of
        Left (e :: SomeException) -> liftIO $ throwIO e
        Right ()                  -> error "Unexpected termination"
  where
    connect' sockaddr tv chan = do
        atomically $ writeTChan chan $ PeerManager.Connected infoHash sockaddr tv
    disconnect' sockaddr chan = do
        atomically $ writeTChan chan $ PeerManager.Disconnected infoHash sockaddr
