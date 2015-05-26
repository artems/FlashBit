module Torrent.Announce.Test (tests) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Torrent.BCode
import Torrent.Announce


tests :: TestTree
tests = testGroup "Torrent.Announce" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testBubbleAnnounce
    , testParseResponse
    ]

testBubbleAnnounce :: TestTree
testBubbleAnnounce = testCase "bubbleAnnounce" $ do
    let url = B8.pack "backup2"
        tier = [B8.pack "backup1", B8.pack "backup2"]
        announce = [[B8.pack "tacker1"], [B8.pack "backup1", B8.pack "backup2"]]
    bubbleAnnounce url tier announce @?=
        [[B8.pack "tacker1"], [B8.pack "backup2", B8.pack "backup1"]]

testParseResponse :: TestTree
testParseResponse = testCase "parseResponse: peers as empty bdict" $ do
    let response = BDict . M.fromList $
            [ (B8.pack "complete", BInt 1)
            , (B8.pack "incomplete", BInt 2)
            , (B8.pack "interval", BInt 1800)
            , (B8.pack "peers", BDict M.empty)
            ]
    parseResponse response @?= TrackerResponse [] (Just 1) (Just 2) 1800 Nothing
