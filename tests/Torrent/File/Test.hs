{-# LANGUAGE ScopedTypeVariables #-}

module Torrent.File.Test (tests) where

import qualified Data.ByteString as B
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (bracket)
import System.IO
import System.Directory (removeFile)

import Torrent.File
import Torrent.Piece

import qualified TestData.SingleTorrent as ST

-- TODO%
-- * read/write on edges of files

tests :: TestTree
tests = testGroup "Torrent.File" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testReadBlock
    , testWriteBlock
    , testCheckPiece
    , testCheckTorrent
    , testOpenTarget
    , testBytesLeft
    ]

testReadBlock :: TestTree
testReadBlock = testCase "readBlock" $ withCompleteTorrent $ \file -> do
    let bs = B.pack [0x00, 0x00, 0x49, 0x49, 0x2A]
        block = PieceBlock 10 5
    result <- readBlock file ST.piece0 block
    result @?= bs

testWriteBlock :: TestTree
testWriteBlock = testCase "writeBlock" $ withTempTorrent $ \handle file -> do
    let bs = B.pack [0x01, 0x02, 0x03, 0x04, 0x05]
        block = PieceBlock 10 5
    writeBlock file ST.piece0 block bs

    hSeek handle AbsoluteSeek 0
    result <- B.hGet handle 100

    (B.take 5 . B.drop 10 $ result) @?= bs

testCheckPiece :: TestTree
testCheckPiece = testGroup "checkPiece"
    [ testCase "complete" $ withCompleteTorrent $ \file -> do
        result <- checkPiece file ST.piece0
        result @?= True
    , testCase "incomplete" $ withInCompleteTorrent $ \file -> do
        result <- checkPiece file ST.piece0
        result @?= False
    ]

testCheckTorrent :: TestTree
testCheckTorrent = testGroup "checkTorrent"
    [ testCase "complete" $ withCompleteTorrent $ \file -> do
        pieceHaveMap <- checkTorrent file ST.pieceArray
        M.foldr (&&) True pieceHaveMap @?= True
    , testCase "incomplete" $ withInCompleteTorrent $ \file -> do
        pieceHaveMap <- checkTorrent file ST.pieceArray
        M.foldr (&&) True pieceHaveMap @?= False
    ]

testOpenTarget :: TestTree
testOpenTarget = testCase "openTarget" $ do
    bc <- openTorrent "tests/_data/cat.torrent"
    (_files, pieceArray, _pieceHaveMap) <- openTarget "tests/_data/incomplete/" bc
    pieceArray @?= ST.pieceArray

testBytesLeft :: TestTree
testBytesLeft = testCase "bytesLeft" $ do
    let pieceHaveMap = M.fromList [(0, False), (1, True), (2, True)]
    bytesLeft ST.pieceArray pieceHaveMap @?= 16384

withCompleteTorrent :: (FileRec -> IO a) -> IO a
withCompleteTorrent action = do
    withFile "tests/_data/complete/cat1.jpg" ReadMode $ \handle ->
        action $ FileRec [(handle, ST.infoLength)]

withInCompleteTorrent :: (FileRec -> IO a) -> IO a
withInCompleteTorrent action = do
    withFile "tests/_data/incomplete/cat1.jpg" ReadMode $ \handle ->
        action $ FileRec [(handle, ST.infoLength)]

withTempTorrent :: (Handle -> FileRec -> IO a) -> IO a
withTempTorrent action =
    bracket
        (openTempFile "tests/_data/incomplete/" "cat.jpg")
        (\(filename, handle) -> hClose handle >> removeFile filename)
        (\(_, handle) -> do
            B.hPut handle $ B.pack $ take (fromIntegral ST.infoLength) [0, 0..]
            action handle $ FileRec [(handle, ST.infoLength)]
        )
