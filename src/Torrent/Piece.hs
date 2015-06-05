module Torrent.Piece
    ( PieceRec(..)
    , PieceNum
    , PieceSize
    , PieceMode(..)
    , PieceBlock(..)
    , PieceBlockOffset
    , PieceBlockLength
    , PieceArray
    , PieceHaveMap
    ) where

import           Data.Array (Array)
import qualified Data.Map as M
import qualified Data.ByteString as B


data PieceRec = PieceRec
    { _pieceOffset   :: Integer
    , _pieceLength   :: Integer
    , _pieceChecksum :: B.ByteString
    } deriving (Eq, Show)

type PieceNum = Integer

type PieceSize = Integer

data PieceMode = Leech | Endgame

data PieceBlock = PieceBlock
    { _blockOffset :: PieceBlockOffset
    , _blockLength :: PieceBlockLength
    } deriving (Eq, Show, Ord)

type PieceBlockOffset = Integer

type PieceBlockLength = Integer

type PieceArray = Array PieceNum PieceRec

type PieceHaveMap = M.Map PieceNum Bool
