module Torrent.Tracker.Test (tests) where

import qualified Network.Socket as S
import qualified Data.ByteString as B

import Test.Tasty
import Test.Tasty.HUnit

import Torrent.BCode as BCode
import Torrent.Tracker


-- TODO:
-- * trackerError
-- * trackerWarning
-- * trackerMinInterval
-- * trackerPeers ip_v6

tests :: TestTree
tests = testGroup "Torrent.TrackerResponse" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "trackerComplete" $ withTrackerResponse $ \bc ->
        trackerComplete bc @?= Just 325
    , testCase "trackerIncomplete" $ withTrackerResponse $ \bc ->
        trackerIncomplete bc @?= Just 67
    , testCase "trackerInterval" $ withTrackerResponse $ \bc ->
        trackerInterval bc @?= Just 1800
    , testCase "trackerPeers" $ withTrackerResponse $ \bc ->
        trackerPeers bc @?= Just
            [ S.SockAddrInet (S.PortNum 0xD5C8) 0x9DE7E66D
            , S.SockAddrInet (S.PortNum 0x1FD8) 0xB2782DCC
            , S.SockAddrInet (S.PortNum 0xF31A) 0x946328BC
            , S.SockAddrInet (S.PortNum 0x541B) 0x62D90732
            , S.SockAddrInet (S.PortNum 0xE453) 0x49A08D4F
            ]
    ]

-- utils
withTrackerResponse :: (BCode -> IO a) -> IO a
withTrackerResponse action = setUp >>= action
  where
    path = "tests/_data/tracker.bencode"
    setUp = do
        content <- B.readFile path
        let (Right bc) = BCode.decode content
        return bc
