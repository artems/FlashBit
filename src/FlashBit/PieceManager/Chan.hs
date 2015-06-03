module FlashBit.PieceManager.Chan
    ( PieceManagerMessage(..)
    , PieceBroadcastMessage(..)
    ) where

import Control.Concurrent.STM
import qualified Data.PieceSet as PS
import qualified Data.ByteString as B

import Torrent

data PieceManagerMessage
    = GrabBlock Integer PS.PieceSet (TMVar (TorrentPieceMode, [(PieceNum, PieceBlock)]))
    | GetCompleted (TMVar [PieceNum])
    | PeerHave [PieceNum] (TMVar [PieceNum])
    | StoreBlock PieceNum PieceBlock B.ByteString
    | PutbackBlock [(PieceNum, PieceBlock)]

data PieceBroadcastMessage
    = PieceComplete PieceNum
    | BlockComplete PieceNum PieceBlock
    | TorrentComplete
