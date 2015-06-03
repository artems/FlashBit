module FlashBit.Listen
    ( runListen
    ) where

import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import Data.Word (Word16)
import qualified Network as N
import qualified Network.Socket as S

import Process
import qualified FlashBit.PeerManager.Chan as PeerManager


data PConf = PConf
    { _localPort       :: Word16
    , _peerManagerChan :: TChan PeerManager.PeerManagerMessage
    }

instance ProcessName PConf where
    processName _ = "Listen"

type PState = ()


runListen :: Word16
          -> TChan PeerManager.PeerManagerMessage 
          -> IO ()
runListen localPort peerManagerChan = do
    let pconf = PConf localPort peerManagerChan
    wrapProcess pconf () process

process :: Process PConf PState ()
process = do
    socket <- asks _localPort >>= listen
    acceptLoop socket

listen :: Word16 -> Process PConf PState S.Socket
listen localPort = do
    let port = N.PortNumber $ fromIntegral localPort
    liftIO $ N.listenOn port

acceptLoop :: S.Socket -> Process PConf PState ()
acceptLoop socket = do
    chan <- asks _peerManagerChan
    conn <- liftIO $ S.accept socket
    let message = PeerManager.NewConnection conn
    liftIO . atomically $ writeTChan chan message
    acceptLoop socket
