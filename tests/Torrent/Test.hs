{-# LANGUAGE ScopedTypeVariables #-}

module Torrent.Test (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import System.Random

import Torrent

import qualified TestData.MultiTorrent as MT
import qualified TestData.SingleTorrent as ST


newtype PeerId' = PeerId' { unPeerId' :: PeerId }
    deriving (Eq, Show)

instance QC.Arbitrary PeerId' where
    arbitrary = do
        stdgen  <- mkStdGen `fmap` QC.arbitrary
        version <- QC.arbitrary
        return $ PeerId' (mkPeerId stdgen version)

tests :: TestTree
tests = testGroup "Torrent" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "length peer_id == 20" $
        \(peerId :: PeerId') -> (length . unPeerId') peerId == 20
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "single"
        [ testCase "mkTorrent" $ ST.withTorrentFile $ \bc ->
            mkTorrent bc @?=
                Just (Torrent ST.infoHash ST.infoPieceCount ST.announceList)
        , testCase "mkPieceArray" $ ST.withTorrentFile $ \bc -> do
            mkPieceArray bc @?=
                Just ST.pieceArray
        ]
    , testGroup "multi"
        [ testCase "mkTorrent" $ MT.withTorrentFile $ \bc ->
            mkTorrent bc @?=
                Just (Torrent MT.infoHash MT.infoPieceCount MT.announceList)
        , testCase "mkPieceArray" $ MT.withTorrentFile $ \bc -> do
            mkPieceArray bc @?=
                Just MT.pieceArray
        ]
    ]
