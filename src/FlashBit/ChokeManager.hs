module FlashBit.ChokeManager
    ( runChokeManager
    , ChokeManagerMessage(..)
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Timer
import qualified Control.Monad.Reader as R
import Control.Monad.Trans (liftIO)
import Data.List
import Data.Function (on)
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import qualified Network.Socket as S
import System.Random
import System.Random.Shuffle

import Process
import FlashBit.PeerDatabase (PeerDatabase)
import FlashBit.TorrentDatabase
import FlashBit.Peer.Common
import qualified FlashBit.PeerDatabase as PeerDatabase


data PConf = PConf
    { _baseSlots        :: Int -- Кол-во активных соединений
    , _optimisticSlots  :: Int
    , _roundTime        :: Int -- Кол-во секунд до след. раунда
    , _peerDatabase     :: PeerDatabase.PeerDatabaseTVar
    , _torrentTV        :: TorrentTVar
    , _chokeManagerChan :: TChan ChokeManagerMessage
    }

instance ProcessName PConf where
    processName _ = "ChokeManager"

type PState = ()

data ChokeManagerMessage = Tick

runChokeManager :: PeerDatabase.PeerDatabaseTVar
                -> TorrentTVar
                -> TChan ChokeManagerMessage
                -> IO ()
runChokeManager peerDatabase torrentTV chokeManagerChan = do
    let pconf  = PConf 2 2 10 peerDatabase torrentTV chokeManagerChan
        pstate = ()
    wrapProcess pconf pstate (setNextTick >> process)

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState ChokeManagerMessage
wait = do
    chokeManagerChan <- R.asks _chokeManagerChan
    liftIO . atomically $ readTChan chokeManagerChan

receive :: ChokeManagerMessage -> Process PConf PState ()
receive message =
    case message of
        Tick -> tick

tick :: Process PConf PState ()
tick = chokeRound >> setNextTick

setNextTick :: Process PConf PState ()
setNextTick = do
    roundTime <- R.asks _roundTime
    chokeChan <- R.asks _chokeManagerChan
    let action = atomically $ writeTChan chokeChan Tick
    _ <- liftIO $ setTimeout roundTime action
    return ()

getPeerDatabase :: Process PConf PState PeerDatabase
getPeerDatabase = do
    peerDatabase <- R.asks _peerDatabase
    liftIO $ PeerDatabase.freeze peerDatabase

getPeerChain :: PeerDatabase -> Process PConf PState [S.SockAddr]
getPeerChain peerDatabase = do
    g <- liftIO newStdGen
    let peers = M.keys peerDatabase
    return $ shuffle' peers (length peers) g

chokeRound :: Process PConf PState ()
chokeRound = do
    peerDb <- getPeerDatabase
    chain  <- getPeerChain peerDb
    let (seeders, leechers) = splitByType peerDb chain
    electedPeers <- selectPeers peerDb seeders leechers
    performChokingUnchoking peerDb chain electedPeers

splitByType :: PeerDatabase -> [S.SockAddr]
            -> ([S.SockAddr], [S.SockAddr])
splitByType peerDb chain = foldl splitter ([], []) chain
  where
    splitter (seeders, leechers) sockaddr =
        case M.lookup sockaddr peerDb of
            Nothing -> (seeders, leechers)
            Just peer
                | PeerDatabase._weInterestedInPeer peer
                    -> (seeders, sockaddr : leechers)
                | PeerDatabase._peerInterestedInUs peer
                    -> (sockaddr : seeders, leechers)
                | otherwise
                    -> (seeders, leechers)

selectPeers :: PeerDatabase
            -> [S.SockAddr] -> [S.SockAddr]
            -> Process PConf PState (S.Set S.SockAddr)
selectPeers peerDb seeders leechers = do
    slots <- R.asks _baseSlots
    let (seedersSlots, leechersSlots) = assignSlots slots (length seeders) (length leechers)
        seeders'  = S.fromList . take seedersSlots $ sortSeeders peerDb seeders
        leechers' = S.fromList . take leechersSlots $ sortLeechers peerDb leechers
    return $ S.union seeders' leechers'

assignSlots :: Int -> Int -> Int -> (Int, Int)
assignSlots slots numOfSeeders numOfLeechers
    | uploadSlots < numOfSeeders && downloadSlots < numOfLeechers
        = (uploadSlots, downloadSlots)
    | uploadSlots >= numOfSeeders && downloadSlots >= numOfLeechers
        = (numOfSeeders, numOfLeechers)
    | uploadSlots >= numOfSeeders && downloadSlots < numOfLeechers
        = (numOfSeeders, min numOfLeechers downloadSlots')
    | uploadSlots < numOfSeeders && downloadSlots >= numOfLeechers
        = (min numOfSeeders uploadSlots', numOfLeechers)
    | otherwise
        = error "assignSlots: impossible"
  where
    round' :: Int -> Double -> Int
    round' n fraction = max 1 $ round (fromIntegral n * fraction)

    uploadSlots   = round' slots 0.3
    downloadSlots = round' slots 0.7

    uploadSlots'   = uploadSlots + downloadSlots - numOfLeechers
    downloadSlots' = downloadSlots + uploadSlots - numOfSeeders

sortSeeders :: PeerDatabase -> [S.SockAddr] -> [S.SockAddr]
sortSeeders peerDb = sortBy (flip compare `on` uploadSpeed)
  where
    uploadSpeed sockaddr = case M.lookup sockaddr peerDb of
        Nothing   -> 0
        Just peer -> PeerDatabase._upSpeed peer

sortLeechers :: PeerDatabase -> [S.SockAddr] -> [S.SockAddr]
sortLeechers peerDb = sortBy (flip compare `on` downloadSpeed)
  where
    downloadSpeed sockaddr = case M.lookup sockaddr peerDb of
        Nothing   -> 0
        Just peer -> PeerDatabase._dnSpeed peer

performChokingUnchoking :: PeerDatabase -> [S.SockAddr] -> S.Set S.SockAddr
                        -> Process PConf PState ()
performChokingUnchoking peerDb chain elected = do
    optimisticSlots <- R.asks _optimisticSlots
    let restPeers = filter (flip S.notMember elected) chain
    F.foldlM (\_ p -> unchoke p) () elected
    optChoke optimisticSlots restPeers
  where
    choke p = do
        debugP $ "choking: " ++ show p
        let peer = fromJust $ M.lookup p peerDb
        let peerChan = PeerDatabase._peerChan peer
        liftIO . atomically $ writeTChan peerChan $ FromChokeManager False
    unchoke p = do
        debugP $ "unchoking: " ++ show p
        let peer = fromJust $ M.lookup p peerDb
        let peerChan = PeerDatabase._peerChan peer
        liftIO . atomically $ writeTChan peerChan $ FromChokeManager False

    optChoke _ []              = return ()
    optChoke 0 (sockaddr : ps) =
        case M.lookup sockaddr peerDb of
            Nothing -> error "impossible (1)"
            Just _  -> choke sockaddr >> optChoke 0 ps
    optChoke k (sockaddr : ps) =
        case M.lookup sockaddr peerDb of
            Nothing -> error "impossible (2)"
            Just _  -> unchoke sockaddr >> optChoke (k - 1) ps
