module Process.TorrentManager
    ( runTorrentManager
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Torrent

import Process
import Process.Status
import Process.PeerManager
import Process.ChokeManager


data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "TorrentManager"


data PState = PState ()


runTorrentManager :: PeerId -> TVar [UpDownStat]
    -> TChan StatusMessage
    -> TChan PeerManagerMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runTorrentManager peerId statV statusChan peerMChan chokeMChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


