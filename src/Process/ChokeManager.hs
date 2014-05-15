module Process.ChokeManager
    ( ChokeManagerMessage(..)
    -- , runChokeManager
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)

import Data.List (partition, foldl', sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T

import System.Random

import Timer
import Torrent
import Process
import Process.Channel


data ChokeManagerMessage
    = ChokeManagerTick
      -- ^ Request that we run another round
    | ChokeManagerAddPeer InfoHash ThreadId (TChan PeerHandlerMessage)
      -- ^ Request that this peer is added
    | ChokeManagerRemovePeer ThreadId
      -- ^ Request that this peer is removed
    | ChokeManagerPieceDone InfoHash PieceNum
      -- ^ Note that a given piece is done
    | ChokeManagerBlockComplete InfoHash PieceNum PieceBlock
      -- ^ Note that a block is complete (endgame)
    | ChokeManagerTorrentComplete InfoHash
      -- ^ Note that the torrent in question is complete


