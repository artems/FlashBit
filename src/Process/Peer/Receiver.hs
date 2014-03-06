module Process.Peer.Receiver
    ( runReceiver
    , specReceiver
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SB


import Process
import Supervisor
import Protocol.Peer

import Process.Peer.Chan



data PConf = PConf
    { cSocket :: Socket
    , cMessageChan :: TChan PeerMessage
    }


runReceiver :: Socket -> TChan PeerMessage -> IO Reason
runReceiver sock chan = do
    execProcess conf () receive
    return Normal
  where
    conf = PConf sock chan


specReceiver :: Socket -> TChan PeerMessage -> IO ChildSpec
specReceiver sock chan = do
    return $ ChildSpec
        { csType = Worker
        , csAction = runReceiver sock chan
        , csRestart = Transient
        , csShutdown = return ()
        , csShutdownTimeout = 100
        }


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

