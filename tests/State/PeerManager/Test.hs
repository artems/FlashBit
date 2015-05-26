module State.PeerManager.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as B
import Control.Monad (forM_)
import qualified Control.Monad.State as S
import qualified Network.Socket as S

import State.PeerManager
import Torrent.Peer


tests :: TestTree
tests = testGroup "State.PeerManager" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testMayIAcceptIncomingPeer
    , testNextPackOfPeers
    ]

testMayIAcceptIncomingPeer :: TestTree
testMayIAcceptIncomingPeer = testGroup "mayIAcceptIncomingPeer"
    [ testCase "empty" $ do
        let script = mayIAcceptIncomingPeer
        exec script @?= True
    , testCase "full" $ do
        let script = do
                forM_ [1..10] $ \x -> addPeer B.empty (S.SockAddrInet 0 x)
                mayIAcceptIncomingPeer
        exec script @?= False
    ]

testNextPackOfPeers :: TestTree
testNextPackOfPeers = testGroup "nextPackOfPeers"
    [ testCase "empty queue" $ do
        let script = do
                pack <- nextPackOfPeers
                return (length pack)
        exec script @?= 0
    , testCase "pull peers" $ do
        let script = do
                let peers = map (\x -> Peer (S.SockAddrInet 0 x)) [1..15]
                enqueuePeers B.empty peers
                pack1 <- nextPackOfPeers
                pack2 <- nextPackOfPeers
                return (length pack1, length pack2)
        exec script @?= (10, 5)
    , testCase "keep maximum" $ do
        let script = do
                let peers = map (\x -> Peer (S.SockAddrInet 0 x)) [1..15]
                enqueuePeers B.empty peers
                pack1 <- nextPackOfPeers
                forM_ [1..8] $ \x -> addPeer B.empty (S.SockAddrInet 0 x)
                pack2 <- nextPackOfPeers
                return (length pack1, length pack2)
        exec script @?= (10, 2)
    ]

exec :: S.State PeerManagerState a -> a
exec action = S.evalState action $ mkPeerManagerState
