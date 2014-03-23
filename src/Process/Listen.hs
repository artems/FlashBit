module Process.Listen
    ( runListen
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Data.Word (Word16)


import Process
import Process.PeerManager


data PConf = PConf ()

instance ProcessName PConf where
    processName _ = "Listen"


data PState = PState ()


runListen :: Word16 -> TChan PeerManagerMessage -> IO ()
runListen port peerMChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process


