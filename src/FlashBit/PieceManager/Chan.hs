module FlashBit.PieceManager.Chan
    ( PieceManagerMessage(..)
    , PieceBroadcastMessage(..)
    ) where

import Control.Concurrent.STM
import qualified Data.Set as S
import qualified Data.ByteString as B

import Torrent

data PieceManagerMessage
    = GrabBlock Integer (S.Set PieceNum) (TMVar (PieceMode, [(PieceNum, PieceBlock)]))
    | GetCompleted (TMVar [PieceNum])
    | PeerHave [PieceNum] (TMVar [PieceNum])
    | PeerUnhave [PieceNum]
    | StoreBlock PieceNum PieceBlock B.ByteString
    | PutbackBlock [(PieceNum, PieceBlock)]

data PieceBroadcastMessage
    = PieceComplete PieceNum
    | BlockComplete PieceNum PieceBlock
    | TorrentComplete
