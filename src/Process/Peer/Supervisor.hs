module Process.Peer.Supervisor
    ( runPeer
    ) where


import Data.Word
import Control.Concurrent.STM
import Network.Socket (Socket)
import qualified Network.Socket as S


import Server
import Process
import Supervisor
import Protocol.Peer

import Process.Peer.Chan
import Process.Peer.Sender (specSender)
import Process.Peer.Handler (specHandler)
import Process.Peer.Receiver (specReceiver)



connect :: S.SockAddr -> IO Socket
connect peer = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect sock peer
    return sock


runPeer :: S.SockAddr -> Handshake -> IO Reason
runPeer peer handshake = do
    sock <- connect peer
    fromChan <- newTChanIO
    sendChan <- newTChanIO
    let specs =
            [ ("sender", specSender sock handshake sendChan)
            , ("handler", specHandler fromChan sendChan)
            , ("receiver", specReceiver sock fromChan)
            ]
    runSupervisor OneForAll 0 90 specs


