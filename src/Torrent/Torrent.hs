module Torrent.Torrent
    ( InfoHash
    , Torrent(..)
    , TorrentState(..)
    , defaultPort
    , defaultBlockSize
    , mkPeerId
    , mkTorrent
    , bytesLeft
    ) where


import Data.Array (array, assocs)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word (Word16)

import System.Random (StdGen, randomRs)

import Torrent.Peer
import Torrent.Piece
import Torrent.BCode (BCode)
import qualified Torrent.BCode as BCode


type InfoHash = B.ByteString


data Torrent = Torrent
    { _torrentInfoHash    :: InfoHash
    , _torrentPieceCount  :: Integer
    , _torrentAnnounceURL :: [[B.ByteString]]
    } deriving (Show)


data TorrentState
    = Seeding
    | Leeching
    deriving (Show)


defaultPort :: Word16
defaultPort = 1680


defaultBlockSize :: PieceBlockSize
defaultBlockSize = 16384 -- bytes


mkPeerId :: String -> StdGen -> PeerId
mkPeerId version gen = header ++ take count randomChars
  where
    count = (20 - length header)
    header = "-FB" ++ version ++ "-"
    randomChars = randomRs ('A', 'Z') gen


mkTorrent :: (B.ByteString -> B.ByteString) -> BCode -> Maybe Torrent
mkTorrent digest bc = do
    infoHash   <- BCode.infoHash digest bc
    announce   <- BCode.announce bc
    pieceCount <- BCode.infoPieceCount bc
    let announceURL = fromMaybe [[announce]] (BCode.announceList bc)
    return Torrent
        { _torrentInfoHash    = infoHash
        , _torrentPieceCount  = pieceCount
        , _torrentAnnounceURL = announceURL
        }


bytesLeft :: PieceArray -> PieceHaveMap -> Integer
bytesLeft pieces haveMap = foldl' f 0 (assocs pieces)
  where
    f acc (pieceNum, piece) =
        case M.lookup pieceNum haveMap of
            Just False -> (_pieceLength piece) + acc
            _          -> acc


