module Process.PeerManager
    ( runPeerManager
    , PeerManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Peer

import Process
import Process.ChokeManager as ChokeManager


data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "PeerManager"


data PState = PState ()


data PeerManagerMessage = PMM


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

