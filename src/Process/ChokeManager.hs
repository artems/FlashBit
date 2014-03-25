module Process.ChokeManager
    ( runChokeManager
    , ChokeManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Torrent
import Process
import Process.Channel


data ChokeManagerMessage
    = ChokeMTick
      -- ^ Request that we run another round
    | ChokeMRemovePeer ThreadId
      -- ^ Request that this peer is removed
    | ChokeMAddPeer InfoHash ThreadId (TChan PeerHandlerMessage)
      -- ^ Request that this peer is added
    | ChokeMPieceDone InfoHash PieceNum
      -- ^ Note that a given piece is done
    | ChokeMBlockComplete InfoHash PieceNum PieceBlock
      -- ^ Note that a block is complete (endgame)
    | ChokeMTorrentComplete InfoHash
      -- ^ Note that the torrent in question is complete



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


