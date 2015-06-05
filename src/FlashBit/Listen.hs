module FlashBit.Listen
    ( runListen
    ) where

import Data.Word (Word16)
import Control.Monad.Reader (liftIO, asks)
import Control.Concurrent.STM
import Control.Exception (finally)
import qualified Network as N
import qualified Network.Socket as S

import Process
import FlashBit.PeerManager.Chan (PeerManagerMessage)
import qualified FlashBit.PeerManager.Chan as PeerManager


data PConf = PConf
    { _localPort       :: Word16
    , _peerManagerChan :: TChan PeerManagerMessage
    }

instance ProcessName PConf where
    processName _ = "Listen"

type PState = ()


runListen :: Word16 -> TChan PeerManagerMessage -> IO ()
runListen localPort peerManagerChan = do
    let pconf  = PConf localPort peerManagerChan
    let pstate = ()
    wrapProcess pconf pstate process

process :: Process PConf PState ()
process = do
    port   <- asks _localPort
    chan   <- asks _peerManagerChan
    socket <- listen port
    liftIO $ finally (accept socket chan) (S.sClose socket)

listen :: Word16 -> Process PConf PState S.Socket
listen localPort = do
    let port = N.PortNumber (fromIntegral localPort)
    liftIO $ N.listenOn port

accept :: S.Socket -> TChan PeerManagerMessage -> IO ()
accept socket chan = do
    conn <- S.accept socket
    let message = PeerManager.NewConnection conn
    atomically $ writeTChan chan message
    accept socket chan
