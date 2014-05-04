module Process.Peer.Sender
    ( runPeerSender
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import qualified Network.Socket as S (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Torrent.Message (Message(KeepAlive), Handshake)
import Torrent.Message (encodeMessage, encodeHandshake)


data PConf = PConf
    { _socket :: S.Socket
    , _messageChan :: TChan Message
    }

instance ProcessName PConf where
    processName _ = "Peer.Sender"

type PState = ()


runPeerSender :: S.Socket -> Handshake -> TChan Message -> IO ()
runPeerSender socket handshake chan = do
    let pconf = PConf socket chan
    wrapProcess pconf () (startup handshake >> process)


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState Message
wait = do
    messageChan <- asks _messageChan
    liftIO . atomically $ readTChan messageChan


receive :: Message -> Process PConf () ()
receive message = sendMessage message


startup :: Handshake -> Process PConf () ()
startup handshake = do
    socket <- asks _socket
    liftIO $ SB.sendAll socket (encodeHandshake handshake)


sendMessage :: Message -> Process PConf () ()
sendMessage message = do
    socket <- asks _socket
    liftIO $ SB.sendAll socket (encodeMessage message)


timeout :: Process PConf () ()
timeout = do
    sendMessage KeepAlive


