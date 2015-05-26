module Torrent.Message.Test (tests) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Test.Tasty
import Test.Tasty.HUnit

import Torrent
import Torrent.Message


tests :: TestTree
tests = testGroup "Torrent.Message" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testMessageSize
    , testHandshakeSize
    , testIncDecode
    , testDecodeMessage
    , testEncodeMessage
    , testDecodeHandshake
    , testEncodeHandshake
    , testDecodeBitField
    , testEncodeBitField
    ]

testMessageSize :: TestTree
testMessageSize = testGroup "messageSize"
    [ testCase "KeepAlive" $
        messageSize KeepAlive @?= 0
    , testCase "Choke" $
        messageSize Choke @?= 1
    , testCase "Unchoke" $
        messageSize Unchoke @?= 1
    , testCase "Interested" $
        messageSize Interested @?= 1
    , testCase "NotInterested" $
        messageSize NotInterested @?= 1
    , testCase "Have" $
        messageSize (Have 250) @?= 5
    , testCase "BitField" $
        messageSize (BitField $ B.pack [0, 0, 0]) @?= 1 + 3
    , testCase "Request" $
        messageSize (Request 1 (PieceBlock 16384 0)) @?= 13
    , testCase "Piece" $
        messageSize (Piece 1 0 (B.pack [0, 0, 0])) @?= 9 + 3
    , testCase "Cancel" $
        messageSize (Cancel 1 (PieceBlock 16384 0)) @?= 13
    , testCase "Port" $
        messageSize (Port 8080) @?= 3
    ]

testHandshakeSize :: TestTree
testHandshakeSize = testCase "handshakeSize" $ handshakeSize @?= 68

testIncDecode :: TestTree
testIncDecode = testCase "inc decode" $ do
    alloca $ \ptr -> do
        poke ptr 0
        result <- decodeMessage (B.pack [0]) $ drain ptr (B.pack [0, 0, 1, 0, 5])
        result @?= (B.empty, 5, Choke)
  where
    drain :: Ptr Int -> B.ByteString -> IO B.ByteString
    drain ptr bs = do
        idx <- peek ptr
        poke ptr (idx + 1)
        return $ B.pack [B.index bs idx]

testDecodeMessage :: TestTree
testDecodeMessage = testGroup "decodeMessage"
    [ testCase "KeepAlive" $ do
        decodeTest [0, 0, 0, 0] KeepAlive
    , testCase "Choke" $ do
        decodeTest [0, 0, 0, 1, 0] Choke
    , testCase "Unchoke" $ do
        decodeTest [0, 0, 0, 1, 1] Unchoke
    , testCase "Interested" $ do
        decodeTest [0, 0, 0, 1, 2] Interested
    , testCase "NotInterested" $ do
        decodeTest [0, 0, 0, 1, 3] NotInterested
    , testCase "Have" $ do
        decodeTest [0, 0, 0, 5, 4, 0, 0, 0, 250]
            (Have 250)
    , testCase "BitField" $
        decodeTest [0, 0, 0, 4, 5, 0, 0, 0]
            (BitField $ B.pack [0, 0, 0])
    , testCase "Request" $
        decodeTest [0, 0, 0, 13, 6, 0, 0, 0, 1, 0, 0, 64, 0, 0, 0, 0, 0]
            (Request 1 (PieceBlock 16384 0))
    , testCase "Piece" $
        decodeTest [0, 0, 0, 12, 7, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
            (Piece 1 0 (B.pack [0, 0, 0]))
    , testCase "Cancel" $
        decodeTest [0, 0, 0, 13, 8, 0, 0, 0, 1, 0, 0, 64, 0, 0, 0, 0, 0]
            (Cancel 1 (PieceBlock 16384 0))
    , testCase "Port" $
        decodeTest [0, 0, 0, 3, 9, 31, 144] (Port 8080)
    ]
  where
    decodeTest bs message = do
        (_, _, message') <- decodeMessage B.empty $ return (B.pack bs)
        message' @?= message

testEncodeMessage :: TestTree
testEncodeMessage = testGroup "encodeMessage"
    [ testCase "KeepAlive" $
        encodeMessage KeepAlive @?= B.pack [0, 0, 0, 0]
    , testCase "Choke" $
        encodeMessage Choke @?= B.pack [0, 0, 0, 1, 0]
    , testCase "Unchoke" $
        encodeMessage Unchoke @?= B.pack [0, 0, 0, 1, 1]
    , testCase "Interested" $
        encodeMessage Interested @?= B.pack [0, 0, 0, 1, 2]
    , testCase "NotInterested" $
        encodeMessage NotInterested @?= B.pack [0, 0, 0, 1, 3]
    , testCase "Have" $
        encodeMessage (Have 250) @?= B.pack [0, 0, 0, 5, 4, 0, 0, 0, 250]
    , testCase "BitField" $
        encodeMessage (BitField $ B.pack [0, 0, 0]) @?=
            B.pack [0, 0, 0, 4, 5, 0, 0, 0]
    , testCase "Request" $
        encodeMessage (Request 1 (PieceBlock 16384 0)) @?=
            B.pack [0, 0, 0, 13, 6, 0, 0, 0, 1, 0, 0, 64, 0, 0, 0, 0, 0]
    , testCase "Piece" $
        encodeMessage (Piece 1 0 (B.pack [0, 0, 0])) @?=
            B.pack [0, 0, 0, 12, 7, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
    , testCase "Cancel" $
        encodeMessage (Cancel 1 (PieceBlock 16384 0)) @?=
            B.pack [0, 0, 0, 13, 8, 0, 0, 0, 1, 0, 0, 64, 0, 0, 0, 0, 0]
    , testCase "Port" $
        encodeMessage (Port 8080) @?= B.pack [0, 0, 0, 3, 9, 31, 144]
    ]

testDecodeHandshake :: TestTree
testDecodeHandshake = testCase "decodeHandshake" $ do
    (_, _, handshake) <- decodeHandshake (return $ handshake1)
    handshake @?= handshake0

testEncodeHandshake :: TestTree
testEncodeHandshake = testCase "encodeHandshake" $
    encodeHandshake handshake0 @?= handshake1

testEncodeBitField :: TestTree
testEncodeBitField = testCase "encodeBitField" $
    -- 1, 3, 5 -> 0101_0100_0000_0000_0000 -> 84 0 0
    encodeBitField 20 [1, 3, 5] @?= B.pack [84, 0, 0]

testDecodeBitField :: TestTree
testDecodeBitField = testCase "decodeBitField" $
    decodeBitField (B.pack [84, 0, 0]) @?= [1, 3, 5]


-- util
handshake0 :: Handshake
handshake0 = Handshake "01234567890123456789" (B8.pack "ABCDEFGHIJKLMNOPQRST") []

handshake1 :: B.ByteString
handshake1 = B.pack
    [ 19,  66, 105, 116, 84,  111, 114, 114, 101, 110
    , 116, 32, 112, 114, 111, 116, 111,  99, 111, 108

    ,  0,  0,  0,  0,  0,  0,  0,  0
    , 65, 66, 67, 68, 69, 70, 71, 72, 73, 74
    , 75, 76, 77, 78, 79, 80, 81, 82, 83, 84
    , 48, 49, 50, 51, 52, 53, 54, 55, 56, 57
    , 48, 49, 50, 51, 52, 53, 54, 55, 56, 57
    ]
