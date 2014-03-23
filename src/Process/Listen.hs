module Process.Listen
    ( runListen
    ) where


import Data.Word (Word16)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)

import Network
import qualified Network.Socket as S

import Process
import Process.PeerManager


data PConf = PConf
    { _port      :: Word16
    , _peerMChan :: TChan PeerManagerMessage
    }

instance ProcessName PConf where
    processName _ = "Listen"

type PState = ()


runListen :: Word16 -> TChan PeerManagerMessage -> IO ()
runListen port peerMChan = do
    let pconf = PConf port peerMChan
    wrapProcess pconf () process


process :: Process PConf PState ()
process = do
    port   <- asks _port
    socket <- listen port
    server socket


listen :: Word16 -> Process PConf PState Socket
listen port = liftIO $ listenOn (PortNumber $ fromIntegral port)


server :: Socket -> Process PConf PState ()
server socket = do
    peerMChan <- asks _peerMChan
    liftIO $ do
        conn <- S.accept socket
        atomically $ writeTChan peerMChan (NewConnection conn)
    server socket


