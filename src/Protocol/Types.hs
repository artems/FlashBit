module Protocol.Types
    ( PeerId
    , InfoHash
    , Piece(..)
    , PieceNum
    , PieceSize
    , PieceBlock(..)
    , PieceBlockSize
    , PieceBlockOffset
    , PieceArray
    , PieceHaveMap
    , Capabilities(..)
    ) where


import Data.Array (Array)
import qualified Data.ByteString as B
import qualified Data.Map as M

type PeerId = String
type InfoHash = B.ByteString

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

data Capabilities
    = Fast
    | Extended
    deriving (Show)

