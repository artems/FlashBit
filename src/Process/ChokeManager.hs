module Process.ChokeManager
    ( runChokeManager
    , ChokeManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Peer
import Piece
import Torrent
import Process
import qualified Process.PeerChan as PeerChan


data ChokeManagerMessage = CMM

data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "ChokeManager"


data PState = PState ()


runChokeManager :: TVar [a] -> TChan ChokeManagerMessage -> IO ()
runChokeManager rateV chokeChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


