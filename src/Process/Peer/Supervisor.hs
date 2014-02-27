module Process.Peer.Supervisor
    ( start
    ) where


import Data.Word
import Control.Concurrent.STM
import Network.Socket (Socket)
import qualified Network.Socket as S

import Protocol.Peer
import Process.Peer.Chan
import qualified Process.Peer.Sender as Sender
import qualified Process.Peer.Handler as Handler
import qualified Process.Peer.Receiver as Receiver

import Server hiding (start)
import Supervisor hiding (start)
import qualified Supervisor


start :: S.SockAddr -> Handshake -> (Reason -> IO ()) -> IO (TMVar Reason)
start peer handshake userTermination = do
    sock <- connect peer
    fromChan <- newTChanIO
    sendChan <- newTChanIO
    let children =
            [ ("sender", sender sock handshake sendChan)
            , ("receiver", receiver sock fromChan)
            , ("handler", handler fromChan sendChan)
            ]
    (stop, _chan) <- Supervisor.start OneForAll 0 0 children userTermination
    return stop


connect :: S.SockAddr -> IO Socket
connect peer = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect sock peer
    return sock


sender :: Socket -> Handshake -> TChan Message -> ChildSpec
sender sock handshake chan = dummyWorker $
    Sender.start sock handshake chan


receiver :: Socket -> TChan PeerMessage -> ChildSpec
receiver sock chan = dummyWorker $
    Receiver.start sock chan


handler :: TChan PeerMessage -> TChan Message -> ChildSpec
handler fromPeerChan sendPeerChan = dummyWorker $
    Handler.start fromPeerChan sendPeerChan


