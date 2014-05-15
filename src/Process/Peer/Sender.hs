module Process.Peer.Sender
    ( runPeerSender
    ) where


import qualified Data.ByteString as B

import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import qualified Network.Socket as S (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Process.Channel
import Torrent.Message (Message(KeepAlive), Handshake)
import Torrent.Message (encodeMessage, encodeHandshake)


data PConf = PConf
    { _socket   :: S.Socket
    , _dropbox  :: TMVar Message
    , _peerChan :: TChan PeerHandlerMessage
    }

instance ProcessName PConf where
    processName _ = "Peer.Sender"

type PState = ()


runPeerSender :: S.Socket -> Handshake -> TMVar Message
              -> TChan PeerHandlerMessage
              -> IO ()
runPeerSender socket handshake dropbox peerChan = do
    let pconf = PConf socket dropbox peerChan
    wrapProcess pconf () (startup handshake >> process)


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState Message
wait = do
    dropbox <- asks _dropbox
    liftIO . atomically $ takeTMVar dropbox


receive :: Message -> Process PConf PState ()
receive message = sendMessage message


startup :: Handshake -> Process PConf PState ()
startup handshake = do
    socket <- asks _socket

    let packet = encodeHandshake handshake
    liftIO $ SB.sendAll socket packet
    reportOnPacketSize packet


sendMessage :: Message -> Process PConf PState ()
sendMessage message = do
    socket <- asks _socket

    let packet = encodeMessage message
    liftIO $ SB.sendAll socket packet
    reportOnPacketSize packet


reportOnPacketSize :: B.ByteString -> Process PConf PState ()
reportOnPacketSize packet = do
    peerChan <- asks _peerChan
    liftIO $ atomically $ writeTChan peerChan $ PeerHandlerFromSender $ fromIntegral (B.length packet)


