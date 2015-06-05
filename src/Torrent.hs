module Torrent
    ( InfoHash
    , AnnounceList
    , Torrent(..)
    , TorrentStatus(..)
    , mkPeerId
    , mkTorrent
    , mkPieceArray
    , pieceArraySize
    , checkPieceNum
    , showInfoHash
    , readInfoHash
    , module Torrent.Peer
    , module Torrent.Piece
    , module Torrent.Exception
    ) where

import qualified Data.Array as A
import qualified Data.ByteString as B
import           Data.Maybe (fromMaybe)
import           Numeric (readHex)
import           Text.Printf (printf)
import           System.Random (StdGen, randomRs)

import           Torrent.Peer
import           Torrent.Piece
import           Torrent.Exception
import           Torrent.BCode (BCode)
import qualified Torrent.Metafile as BCode


type InfoHash = B.ByteString

type AnnounceList = [[B.ByteString]]

data Torrent = Torrent
    { _torrentName         :: B.ByteString
    , _torrentLength       :: Integer
    , _torrentInfoHash     :: InfoHash
    , _torrentInfoFiles    :: [([B.ByteString], Integer)]
    , _torrentPieceCount   :: Integer
    , _torrentAnnounceList :: AnnounceList
    , _torrentComment      :: B.ByteString
    , _torrentCreationDate :: Maybe Integer
    , _torrentPieceArray   :: PieceArray
    } deriving (Eq, Show)

data TorrentStatus = TorrentStatus
    { _torrentIsActive   :: Bool
    , _torrentLeft       :: Integer
    , _torrentUploaded   :: Integer
    , _torrentDownloaded :: Integer
    , _torrentComplete   :: Maybe Integer
    , _torrentIncomplete :: Maybe Integer
    , _torrentPeerStatus :: PeerStatus
    }

mkPeerId :: StdGen -> String -> PeerId
mkPeerId gen version = header ++ take count randomChars
  where
    count = (20 - length header)
    header = "-FB" ++ (take 10 version) ++ "-"
    randomChars = randomRs ('A', 'Z') gen

mkTorrent :: BCode -> Maybe Torrent
mkTorrent bc = do
    name       <- BCode.infoName bc
    hash       <- BCode.infoHash bc
    size       <- BCode.infoLength bc
    files      <- BCode.infoFiles bc
    comment    <- BCode.comment bc
    announce   <- BCode.announce bc
    pieceCount <- BCode.infoPieceCount bc
    pieceArray <- mkPieceArray bc
    let creationDate = BCode.creationDate bc
    let announceList = fromMaybe [[announce]] (BCode.announceList bc)
    return $ Torrent
        { _torrentName         = name
        , _torrentLength       = size
        , _torrentInfoHash     = hash
        , _torrentInfoFiles    = files
        , _torrentComment      = comment
        , _torrentPieceCount   = pieceCount
        , _torrentAnnounceList = announceList
        , _torrentCreationDate = creationDate
        , _torrentPieceArray   = pieceArray
        }

mkPieceArray :: BCode -> Maybe PieceArray
mkPieceArray bc = do
    infoLength  <- BCode.infoLength bc
    pieceData   <- BCode.infoPieces bc
    pieceCount  <- BCode.infoPieceCount bc
    pieceLength <- BCode.infoPieceLength bc
    pieceList   <- sequence $ extract pieceLength infoLength 0 pieceData
    return $ A.array (0, pieceCount - 1) (zip [0..] pieceList)
  where
    extract :: Integer -> Integer -> Integer -> [B.ByteString] -> [Maybe PieceRec]
    extract _ _ _ []  = []
    extract pieceLength remain offset (checksum : xs)
        | remain <= 0 = [Nothing]
        | otherwise   = (Just piece) : nextPiece
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
pieceArraySize = succ . snd . A.bounds

checkPieceNum :: Torrent -> PieceNum -> Bool
checkPieceNum torrent pieceNum = pieceNum >= lo && pieceNum <= hi
  where
    pieceArray = _torrentPieceArray torrent
    (lo, hi)   = A.bounds pieceArray

showInfoHash :: InfoHash -> String
showInfoHash = concatMap (printf "%02X") . B.unpack

readInfoHash :: String -> InfoHash
readInfoHash = B.pack . loop
  where
    loop []           = []
    loop [_]          = error "impossible"
    loop (a : b : xs) = case readHex [a, b] of
        [(w, "")] -> w : loop xs
        _         -> error "impossible"
