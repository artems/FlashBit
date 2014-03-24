module Process.PeerManager
    ( runPeerManager
    , PeerManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import qualified Network.Socket as S

import Torrent
import Process
import Process.Status
import Process.FileAgent
import Process.PieceManager as PieceManager
import Process.ChokeManager as ChokeManager


data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "PeerManager"


data PState = PState ()


data PeerManagerMessage
    = PMM
    | NewConnection (S.Socket, S.SockAddr)
    | AddTorrent InfoHash (TVar [UpDownStat]) (TChan PieceManagerMessage) (TChan FileAgentMessage) PieceArray


runPeerManager :: PeerId -> TVar [a]
    -> TChan PeerManagerMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPeerManager peerId rateV peerMChan chokeMChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


