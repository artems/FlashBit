module TestData.SingleTorrent
    where


import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified TestData as TD

import Torrent
import Torrent.BCode as BCode


infoName :: B.ByteString
infoName = B8.pack "cat1.jpg"

infoHash :: B.ByteString
infoHash = B.pack
    [ 0x7D, 0x3C, 0x04, 0x24, 0x80
    , 0xFD, 0xE5, 0xF0, 0x61, 0x0F
    , 0x6C, 0xB5, 0x8B, 0x15, 0x40
    , 0x28, 0x5B, 0xB1, 0xAD, 0x4E
    ]

infoFiles :: [([B.ByteString], Integer)]
infoFiles = [([B8.pack "cat1.jpg"], 32853)]

infoLength :: Integer
infoLength = 32853

comment :: B.ByteString
comment = B8.pack "Cat Image"

creationDate :: Integer
creationDate = 1394263188

announce :: B.ByteString
announce = B8.pack "http://localhost:80/announce"

announceList :: [[B.ByteString]]
announceList =
    [ [B8.pack "http://localhost:80/announce"]
    , [B8.pack "http://localhost:81/announce"]
    , [B8.pack "http://localhost:82/announce"]
    ]

infoPieceCount :: Integer
infoPieceCount = 3

infoPieceLength :: Integer
infoPieceLength = 16384

piece0_cs :: B.ByteString
piece0_cs = B.pack
    [ 0xFD, 0x40, 0x6C, 0xCF, 0x22
    , 0xA5, 0xFF, 0xDE, 0xA7, 0x48
    , 0xD3, 0xD5, 0x5C, 0x5F, 0xD3
    , 0xB6, 0xFE, 0x69, 0x47, 0x91
    ]

piece1_cs :: B.ByteString
piece1_cs = B.pack
    [ 0xDB, 0x67, 0x37, 0x6A, 0xCF
    , 0xFB, 0x73, 0xD0, 0x80, 0x3E
    , 0x8D, 0xF3, 0xFA, 0x60, 0x8B
    , 0xAA, 0xA2, 0x40, 0x26, 0x3B
    ]

piece2_cs :: B.ByteString
piece2_cs = B.pack
    [ 0xF4, 0xFA, 0xD4, 0xED, 0xEB
    , 0xFB, 0x74, 0xB3, 0x7F, 0x71
    , 0xEA, 0x0F, 0xE7, 0x1D, 0x2A
    , 0xA5, 0xFA, 0x92, 0xEF, 0x0E
    ]

piece0 :: PieceRec
piece0 = PieceRec 0     16384 piece0_cs

piece1 :: PieceRec
piece1 = PieceRec 16384 16384 piece1_cs

piece2 :: PieceRec
piece2 = PieceRec 32768 85    piece2_cs

infoPieces :: [B.ByteString]
infoPieces = [ piece0_cs, piece1_cs, piece2_cs ]

pieceArray :: PieceArray
pieceArray = A.array (0, 2) [(0, piece0), (1, piece1), (2, piece2)]

torrent :: Torrent
torrent = Torrent
    infoName infoLength infoHash infoFiles infoPieceCount
    announceList comment (Just creationDate) pieceArray

withTorrentFile :: (BCode.BCode -> IO a) -> IO a
withTorrentFile = TD.withTorrentFile "tests/_data/cat.torrent"

