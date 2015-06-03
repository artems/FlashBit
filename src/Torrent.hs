module Torrent
    ( module Torrent.Peer
    , module Torrent.Piece
    , InfoHash
    , AnnounceList
    , TorrentMeta(..)
    , TorrentStatus(..)
    , TorrentPieceMode(..)
    , UpDownStat(..)
    , defaultPort
    , defaultBlockSize
    , mkPeerId
    , mkTorrentMeta
    , mkPieceArray
    , pieceArraySize
    , checkPieceNum
    , showInfoHash
    ) where

import Data.Array (array, bounds)
import Data.List (intercalate)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Text.Printf (printf)
import System.Random (StdGen, randomRs)

import Torrent.Peer
import Torrent.Piece
import Torrent.BCode (BCode)
import qualified Torrent.Metafile as BCode


type InfoHash = B.ByteString

type AnnounceList = [[B.ByteString]]

data TorrentMeta = TorrentMeta
    { _torrentMetaInfoHash     :: InfoHash
    , _torrentMetaPieceCount   :: Integer
    , _torrentMetaAnnounceList :: AnnounceList
    } deriving (Eq, Show)

data TorrentStatus = TorrentStatus
    { _torrentLeft       :: Integer
    , _torrentUploaded   :: Integer
    , _torrentDownloaded :: Integer
    , _torrentComplete   :: Maybe Integer
    , _torrentIncomplete :: Maybe Integer
    , _torrentPeerStatus :: PeerStatus
    }

instance Show TorrentStatus where
    show (TorrentStatus left up down complete incomplete state) =
        concat
            [ "left: "          ++ show left        ++ " "
            , "uploaded: "      ++ show up          ++ " "
            , "downloaded: "    ++ show down        ++ " "
            , "complete: "      ++ show complete    ++ " "
            , "incomplete: "    ++ show incomplete  ++ " "
            , "state: "         ++ show state       ++ " "
            ]

data TorrentPieceMode = Leech | Endgame

data UpDownStat = UpDownStat
    { _statInfoHash   :: InfoHash
    , _statUploaded   :: Integer
    , _statDownloaded :: Integer
    }


defaultPort :: Word16
defaultPort = 1369

defaultBlockSize :: PieceBlockLength
defaultBlockSize = 16384 -- bytes

mkPeerId :: StdGen -> String -> PeerId
mkPeerId gen version = header ++ take count randomChars
  where
    count = (20 - length header)
    header = "-FB" ++ (take 10 version) ++ "-"
    randomChars = randomRs ('A', 'Z') gen

mkTorrentMeta :: BCode -> Maybe TorrentMeta
mkTorrentMeta bc = do
    infoHash   <- BCode.infoHash bc
    announce   <- BCode.announce bc
    pieceCount <- BCode.infoPieceCount bc
    let announceList = fromMaybe [[announce]] (BCode.announceList bc)
    return $ TorrentMeta
        { _torrentMetaInfoHash     = infoHash
        , _torrentMetaPieceCount   = pieceCount
        , _torrentMetaAnnounceList = announceList
        }

mkPieceArray :: BCode -> Maybe PieceArray
mkPieceArray bc = do
    infoLength  <- BCode.infoLength bc
    pieceData   <- BCode.infoPieces bc
    pieceCount  <- BCode.infoPieceCount bc
    pieceLength <- BCode.infoPieceLength bc
    let pieceList = extract pieceLength infoLength 0 pieceData
    return $ array (0, pieceCount - 1) (zip [0..] pieceList)
  where
    extract :: Integer -> Integer -> Integer -> [B.ByteString] -> [PieceRec]
    extract _           _          _      []              = []
    extract pieceLength remain offset (checksum : xs)
        | remain <= 0 = error "mkPieceArray: Суммарный размер файлов не равен сумме размеров частей торрента"
        | otherwise   = piece : nextPiece
            where
              piece = PieceRec
                { _pieceOffset   = offset
                , _pieceLength   = min remain pieceLength
                , _pieceChecksum = checksum
                }
              newLength = remain - pieceLength
              newOffset = offset + pieceLength
              nextPiece = extract pieceLength newLength newOffset xs

pieceArraySize :: PieceArray -> Integer
pieceArraySize pieceArray = succ . snd . bounds $ pieceArray

checkPieceNum :: PieceArray -> PieceNum -> Bool
checkPieceNum pieceArray pieceNum = pieceNum >= lo && pieceNum <= hi
  where
    (lo, hi) = bounds pieceArray

showInfoHash :: InfoHash -> String
showInfoHash = intercalate ":" . map (printf "%02X") . B.unpack
