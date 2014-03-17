module Piece
    ( Piece(..)
    , PieceNum
    , PieceSize
    , PieceBlock(..)
    , PieceBlockSize
    , PieceBlockOffset
    , PieceArray
    , PieceHaveMap
    , mkPieceArray
    ) where


import Data.Array (Array, array)
import qualified Data.Map as M
import qualified Data.ByteString as B

import BCode
import qualified BCodeTorrent as BCode


data Piece = Piece
    { pieceOffset :: Integer
    , pieceLength :: Integer
    , pieceChecksum :: B.ByteString
    } deriving (Show)

type PieceNum = Integer

type PieceSize = Integer

data PieceBlock = PieceBlock
    { blockSize   :: PieceBlockSize
    , blockOffset :: PieceBlockOffset
    } deriving (Eq, Show)

type PieceBlockSize = Integer

type PieceBlockOffset = Integer

type PieceArray = Array PieceNum Piece

type PieceHaveMap = M.Map PieceNum Bool


mkPieceArray :: BCode -> Maybe PieceArray
mkPieceArray bc = do
    length' <- BCode.infoLength bc
    pieceData <- BCode.infoPieces bc
    pieceCount <- BCode.infoPieceCount bc
    pieceLength' <- BCode.infoPieceLength bc
    let pieceList = extract pieceLength' length' 0 pieceData
        pieceArray = array (0, pieceCount - 1) (zip [0..] pieceList)
    return pieceArray
  where
    extract :: Integer -> Integer -> Integer -> [B.ByteString] -> [Piece]
    extract _ _ _ [] = []
    extract pieceLength' length' offset (x:xs)
        | length' <= 0
            = error "mkPieceArray: Суммарный размер файлов не равен сумме размеров частей торрента"
        | otherwise = piece : restPieces
            where
              piece = Piece
                { pieceOffset = offset
                , pieceLength = min length' pieceLength'
                , pieceChecksum = x
                }
              newLength = length' - pieceLength'
              newOffset = offset + pieceLength'
              restPieces = extract pieceLength' newLength newOffset xs


