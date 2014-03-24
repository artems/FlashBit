module Process.PieceManager
    ( runPieceManager
    , PieceManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Torrent
import Process
import Process.Status
import Process.FileAgent
import Process.ChokeManager


data PieceManagerMessage = PMM

data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "PieceManager"


data PState = PState ()


runPieceManager :: InfoHash -> PieceArray -> PieceHaveMap
    -> TChan PieceManagerMessage
    -> TChan FileAgentMessage
    -> TChan StatusMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPieceManager infohash pieceArray pieceHaveMap pieceMChan fsChan statusChan chokeChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


