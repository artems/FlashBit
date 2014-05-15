module Process.Peer.Receiver
    ( runPeerReceiver
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)

import qualified Data.ByteString as B

import qualified Network.Socket as S (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Process.Channel
import Torrent.Message (Message, Handshake)
import Torrent.Message (decodeMessage, decodeHandshake)


data PConf = PConf
    { _socket :: S.Socket
    , _chan   :: TChan PeerHandlerMessage
    }

instance ProcessName PConf where
    processName _ = "Peer.Receiver"

type PState = ()


runPeerReceiver :: S.Socket -> TChan PeerHandlerMessage -> IO ()
runPeerReceiver socket chan = do
    let pconf = PConf socket chan
    wrapProcess pconf () receive


receive :: Process PConf () ()
receive = receiveHandshake


receiveHandshake :: Process PConf () ()
receiveHandshake = do
    chan   <- asks _chan
    socket <- asks _socket
    (remain, size, handshake) <- liftIO $ decodeHandshake (demandInput socket)
    liftIO . atomically $ writeTChan chan (PeerHandlerFromPeer (Left handshake) size)
    receiveMessage remain


receiveMessage :: B.ByteString -> Process PConf () ()
receiveMessage remain = do
    chan   <- asks _chan
    socket <- asks _socket
    (remain', size, message) <- liftIO $ decodeMessage remain (demandInput socket)
    liftIO . atomically $ writeTChan chan (PeerHandlerFromPeer (Right message) size)
    receiveMessage remain'


demandInput :: S.Socket -> IO B.ByteString
demandInput socket = do
    packet <- SB.recv socket 1024
    if B.length packet /= 0
        then return packet
        else fail "demandInput: socket dead"


