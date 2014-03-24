module Process.FileAgent
    ( runFileAgent
    , FileAgentMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import FS
import Torrent
import Process
import qualified Process.PeerChan as PeerChan


data FileAgentMessage = FAM

data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "FileAgent"


data PState = PState ()


runFileAgent :: TorrentFile -> PieceArray -> TChan FileAgentMessage -> IO ()
runFileAgent target pieceArray fsChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


