module Process.Peer.Receiver
    ( runPeerReceiver
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.Socket as S (Socket)
import qualified Network.Socket.ByteString as SB


import Process
import Torrent.Message (Message, Handshake)
import Torrent.Message (decodeMessage, decodeHandshake)


data PConf = PConf
    { _socket :: S.Socket
    , _messageChan :: TChan (Either Handshake Message)
    }

instance ProcessName PConf where
    processName _ = "Peer.Receiver"

type PState = ()


runPeerReceiver :: S.Socket -> TChan (Either Handshake Message) -> IO ()
runPeerReceiver socket chan = do
    let pconf = PConf socket chan
    wrapProcess pconf () receive


receive :: Process PConf () ()
receive = receiveHandshake


receiveHandshake :: Process PConf () ()
receiveHandshake = do
    socket      <- asks _socket
    messageChan <- asks _messageChan
    (remain, handshake) <- liftIO $ decodeHandshake (demandInput socket)
    liftIO . atomically $ writeTChan messageChan (Left handshake)
    receiveMessage remain


receiveMessage :: ByteString -> Process PConf () ()
receiveMessage remain = do
    socket      <- asks _socket
    messageChan <- asks _messageChan
    (remain', message) <- liftIO $ decodeMessage remain (demandInput socket)
    liftIO . atomically $ writeTChan messageChan (Right message)
    receiveMessage remain'


demandInput :: S.Socket -> IO ByteString
demandInput socket = do
    packet <- SB.recv socket 1024
    if B.length packet /= 0
        then return packet
        else fail "demandInput: socket dead"


