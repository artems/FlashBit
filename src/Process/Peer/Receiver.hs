module Process.Peer.Receiver
    ( start
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Process.Peer.Chan
import Protocol.Peer
import Server hiding (start)
import qualified Server


data ReceiverMessage = Stoped Reason


start :: Socket -> TChan PeerMessage -> (Reason -> IO ()) -> IO (TMVar Reason)
start sock fromChan userTermination = do
    chan <- newTChanIO
    receiverId <- startReceiver sock fromChan (writeReason chan)
    Server.start receiverId chan server userTermination
  where
    writeReason chan = \reason -> atomically $ writeTChan chan (Stoped reason)



server :: Server ThreadId ReceiverMessage
server = dummyServer
    { srvOnMessage = onMessage
    , srvTerminate = onTerminate
    }

onMessage _state (Stoped reason) = do
    return $ Left reason

onTerminate receiverThreadId _reason = do
    killThread receiverThreadId


data PConf = PConf
    { cSocket :: Socket
    , cMessageChan :: TChan PeerMessage
    }

startReceiver :: Socket -> TChan PeerMessage -> (Reason -> IO ()) -> IO ThreadId
startReceiver sock chan userTermination
    = forkFinally proc termination
  where
    conf = PConf sock chan
    proc = execProcess conf () receive
    termination e = case e of
        Left e  -> userTermination (Exception e)
        Right _ -> userTermination Normal


receive :: Process PConf () ()
receive = receiveHandshake


receiveHandshake :: Process PConf () ()
receiveHandshake = do
    sock <- asks cSocket
    chan <- asks cMessageChan
    (remain, handshake) <- liftIO $ decodeHandshake (demandInput sock)
    liftIO . atomically $ writeTChan chan (PeerHandshake handshake)
    receiveMessage remain


receiveMessage :: ByteString -> Process PConf () ()
receiveMessage remain = do
    sock <- asks cSocket
    chan <- asks cMessageChan
    (remain', message) <- liftIO $ decodeMessage remain (demandInput sock)
    liftIO . atomically $ writeTChan chan (PeerMessage message)
    receiveMessage remain'


demandInput :: Socket -> IO ByteString
demandInput sock = do
    packet <- SB.recv sock 1024
    if B.length packet /= 0
        then return packet
        else fail "demandInput: socket dead"

