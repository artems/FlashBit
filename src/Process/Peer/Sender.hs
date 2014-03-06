module Process.Peer.Sender
    ( runSender
    , specSender
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SB

import Server
import Process
import Supervisor
import Protocol.Peer


data PConf = PConf
    { cSocket :: Socket
    , cStopVar :: TMVar ()
    , cMessageChan :: TChan Message
    }


runSender :: Socket -> Handshake -> TChan Message -> TMVar () -> IO Reason
runSender sock handshake chan stop =
    runServer conf () (startup handshake) server
  where
    conf = PConf sock stop chan
    server = mkServerWithTimeout wait receive terminate (60000 * 1000) timeout
    terminate = \_ -> return ()


specSender :: Socket -> Handshake -> TChan Message -> IO ChildSpec
specSender sock handshake chan = do
    stop <- newEmptyTMVarIO
    return $ ChildSpec
        { csType = Worker
        , csAction = runSender sock handshake chan stop
        , csRestart = Transient
        , csShutdown = atomically $ putTMVar stop ()
        , csShutdownTimeout = 100
        }


wait :: Process PConf () (Either () Message)
wait = do
    stop <- asks cStopVar
    chan <- asks cMessageChan
    liftIO . atomically $ orElse
        (takeTMVar stop >>= return . Left)
        (readTChan chan >>= return . Right)


startup :: Handshake -> Process PConf () ()
startup handshake = do
    sock <- asks cSocket
    liftIO $ SB.sendAll sock (encodeHandshake handshake)


receive :: Either () Message -> Process PConf () ()
receive (Left _) = stopProcess Shutdown
receive (Right message) = sendMessage message


timeout :: Process PConf () ()
timeout = do
    liftIO . putStrLn $ "send keep-alive"
    sendMessage KeepAlive


sendMessage :: Message -> Process PConf () ()
sendMessage message = do
    sock <- asks cSocket
    liftIO $ SB.sendAll sock (encodeMessage message)


