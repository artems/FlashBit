module Torrent.Metafile.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified TestData.MultiTorrent as MT
import qualified TestData.SingleTorrent as ST

import Torrent.Metafile


tests :: TestTree
tests = testGroup "Torrent.TorrentFile" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testSingleTorrent, testMultiTorrent]

testSingleTorrent :: TestTree
testSingleTorrent = testGroup "single-torrent"
    [ testCase "comment" $ ST.withTorrentFile $ \bc ->
        comment bc @?=
            Just ST.comment
    , testCase "announce" $ ST.withTorrentFile $ \bc ->
        announce bc @?=
            Just ST.announce
    , testCase "announceList" $ ST.withTorrentFile $ \bc ->
        announceList bc @?=
            Just ST.announceList
    , testCase "creationDate" $ ST.withTorrentFile $ \bc ->
        creationDate bc @?=
            Just ST.creationDate
    , testCase "infoName" $ ST.withTorrentFile $ \bc ->
        infoName bc @?=
            Just ST.infoName
    , testCase "infoHash" $ ST.withTorrentFile $ \bc ->
        infoHash bc @?=
            Just ST.infoHash
    , testCase "infoFiles" $ ST.withTorrentFile $ \bc ->
        infoFiles bc @?=
            Just ST.infoFiles
    , testCase "infoLength" $ ST.withTorrentFile $ \bc ->
        infoLength bc @?=
            Just ST.infoLength
    , testCase "infoPieceCount" $ ST.withTorrentFile $ \bc ->
        infoPieceCount bc @?=
            Just ST.infoPieceCount
    , testCase "infoPieceLength" $ ST.withTorrentFile $ \bc ->
        infoPieceLength bc @?=
            Just ST.infoPieceLength
    , testCase "infoPieces" $ ST.withTorrentFile $ \bc ->
        infoPieces bc @?=
            Just ST.infoPieces
    ]

testMultiTorrent :: TestTree
testMultiTorrent = testGroup "multi-torrent"
    [ testCase "comment" $ MT.withTorrentFile $ \bc ->
        comment bc @?=
            Just MT.comment
    , testCase "announce" $ MT.withTorrentFile $ \bc ->
        announce bc @?=
            Just MT.announce
    , testCase "announceList" $ MT.withTorrentFile $ \bc ->
        announceList bc @?=
            Nothing
    , testCase "creationDate" $ MT.withTorrentFile $ \bc ->
        creationDate bc @?=
            Just MT.creationDate
    , testCase "infoName" $ MT.withTorrentFile $ \bc ->
        infoName bc @?=
            Just MT.infoName
    , testCase "infoHash" $ MT.withTorrentFile $ \bc ->
        infoHash bc @?=
            Just MT.infoHash
    , testCase "infoFiles" $ MT.withTorrentFile $ \bc ->
        infoFiles bc @?=
            Just MT.infoFiles
    , testCase "infoLength" $ MT.withTorrentFile $ \bc ->
        infoLength bc @?=
            Just MT.infoLength
    , testCase "infoPieceCount" $ MT.withTorrentFile $ \bc ->
        infoPieceCount bc @?=
            Just MT.infoPieceCount
    , testCase "infoPieceLength" $ MT.withTorrentFile $ \bc ->
        infoPieceLength bc @?=
            Just MT.infoPieceLength
    ]
