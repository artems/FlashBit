module TestData.MultiTorrent
    where


import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified TestData as TD

import Torrent
import Torrent.BCode as BCode


infoName :: B.ByteString
infoName = B8.pack "kitten"

infoHash :: B.ByteString
infoHash = B.pack
    [ 0x2A, 0x1C, 0xEE, 0xBE, 0xB4
    , 0xD1, 0x14, 0xBA, 0x8D, 0xFB
    , 0x7C, 0x4F, 0x20, 0x59, 0x90
    , 0x07, 0xC5, 0xDE, 0xC2, 0xC8
    ]

infoFiles :: [([B.ByteString], Integer)]
infoFiles =
    [ ([B8.pack "cat2.jpg"], 86195)
    , ([B8.pack "cat1.jpg"], 32853)
    , ([B8.pack "cat3.jpg"], 22215)
    ]

infoLength :: Integer
infoLength = 141263

comment :: B.ByteString
comment = B8.pack "Kitten Public Domain"

creationDate :: Integer
creationDate = 1394263123

announce :: B.ByteString
announce = B8.pack "http://localhost:80/announce"

announceList :: [[B.ByteString]]
announceList = [[B8.pack "http://localhost:80/announce"]]

infoPieceCount :: Integer
infoPieceCount = 5

infoPieceLength :: Integer
infoPieceLength = 32768

piece0_cs :: B.ByteString
piece0_cs = B.pack
    [ 0x41, 0x4B, 0xFD, 0xCA, 0xF8
    , 0x10, 0xD4, 0xD5, 0xA9, 0xB6
    , 0x7A, 0x38, 0xD4, 0xEE, 0x6A
    , 0xB0, 0x1B, 0xEB, 0xDB, 0xBE
    ]

piece1_cs :: B.ByteString
piece1_cs = B.pack
    [ 0xD4, 0x2C, 0xC6, 0xC2, 0xE1
    , 0x8B, 0x2C, 0xB6, 0x45, 0x88
    , 0x70, 0xF0, 0x58, 0xDB, 0xCF
    , 0xCC, 0x9E, 0x91, 0x30, 0xBC
    ]

piece2_cs :: B.ByteString
piece2_cs = B.pack
    [ 0xC3, 0x18, 0x3B, 0xE5, 0x0B
    , 0xD0, 0x2A, 0x7C, 0x4A, 0x29
    , 0x19, 0x2D, 0xF2, 0x19, 0x9A
    , 0x61, 0x8E, 0xAD, 0x64, 0x9A
    ]

piece3_cs :: B.ByteString
piece3_cs = B.pack
    [ 0x81, 0xC9, 0xD5, 0x3C, 0xDC
    , 0xC6, 0x28, 0x1F, 0x76, 0xBE
    , 0x11, 0x4D, 0xE3, 0x70, 0x31
    , 0x9E, 0x02, 0x0F, 0x0B, 0x96
    ]

piece4_cs :: B.ByteString
piece4_cs = B.pack
    [ 0xD8, 0xF5, 0x87, 0x89, 0xE4
    , 0x43, 0xC0, 0xE6, 0xEC, 0x5C
    , 0xBD, 0xFB, 0x3B, 0x26, 0xF6
    , 0x47, 0xD1, 0xE3, 0x31, 0x12
    ]

infoPieces :: [B.ByteString]
infoPieces = [ piece0_cs, piece1_cs, piece2_cs, piece3_cs, piece4_cs ]

pieceArray :: PieceArray
pieceArray = A.array (0, 4)
    [ (0, PieceRec 0      32768 piece0_cs)
    , (1, PieceRec 32768  32768 piece1_cs)
    , (2, PieceRec 65536  32768 piece2_cs)
    , (3, PieceRec 98304  32768 piece3_cs)
    , (4, PieceRec 131072 10191 piece4_cs)
    ]

withTorrentFile :: (BCode.BCode -> IO a) -> IO a
withTorrentFile = TD.withTorrentFile "tests/_data/kitten.torrent"

