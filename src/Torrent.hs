module Torrent
    ( Torrent(..)
    , TorrentState(..)
    , defaultPort
    , defaultBlockSize
    , bytesLeft
    , mkPeerId
    , mkTorrent
    , mkPieceArray
    ) where


import Data.Array (array, assocs)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word (Word16)
import System.Random (StdGen, randomRs)


import BCode (BCode)
import qualified BCodeTorrent as BCode
import Version (protoVersion)
import Protocol


data Torrent = Torrent
    { torrentInfoHash :: InfoHash
    , torrentPieceCount :: Integer
    , torrentAnnounceURL :: [[B.ByteString]]
    } deriving (Show)

data TorrentState
    = Seeding
    | Leeching
    deriving (Show)


defaultPort :: Word16
defaultPort = 1579


defaultBlockSize :: PieceBlockSize
defaultBlockSize = 16384 -- bytes


bytesLeft :: PieceArray -> PieceHaveMap -> Integer
bytesLeft pieces haveMap = foldl' f 0 (assocs pieces)
  where
    f acc (pieceNum, piece) =
        case M.lookup pieceNum haveMap of
            Just False -> (pieceLength piece) + acc
            _          -> acc


mkPeerId :: StdGen -> PeerId
mkPeerId gen = header ++ take count randomChars
  where
    header = "-FB" ++ protoVersion ++ "-"
    count  = (20 - length header)
    randomChars = randomRs ('A', 'Z') gen


mkTorrent :: BCode -> Maybe Torrent
mkTorrent bc = do
    infoHash <- BCode.infoHash bc
    announce <- BCode.announce bc
    pieceCount <- BCode.infoPieceCount bc
    let announceURL = fromMaybe [[announce]] (BCode.announceList bc)
    return Torrent
        { torrentInfoHash = infoHash
        , torrentPieceCount = pieceCount
        , torrentAnnounceURL = announceURL
        }


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


