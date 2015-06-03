module FlashBit.Peer.Receiver
    ( runPeerReceiver
    ) where

import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import qualified Data.ByteString as B
import qualified Network.Socket as S (Socket)

import Process
import FlashBit.Peer.Common
import qualified Torrent.Message as TM


data PConf = PConf
    { _prefix    :: String
    , _socket    :: S.Socket
    , _receiveTV :: TVar Integer
    , _peerChan  :: TChan PeerMessage
    }

instance ProcessName PConf where
    processName pconf = "Peer.Receiver [" ++ _prefix pconf ++ "]"

type PState = ()


runPeerReceiver :: String -> S.Socket -> B.ByteString
                -> TVar Integer
                -> TChan PeerMessage
                -> IO ()
runPeerReceiver prefix socket remain receiveTV peerChan = do
    let pconf = PConf prefix socket receiveTV peerChan
    wrapProcess pconf () (receive remain)


receive :: B.ByteString -> Process PConf PState ()
receive remain = do
    socket    <- asks _socket
    receiveTV <- asks _receiveTV
    peerChan  <- asks _peerChan
    (remain', consumed, message) <- liftIO $ TM.receiveMessage remain socket
    liftIO . atomically $ do
        n <- readTVar receiveTV
        writeTVar receiveTV (n + consumed)
    liftIO . atomically $ writeTChan peerChan (FromPeer message)
    receive remain'
