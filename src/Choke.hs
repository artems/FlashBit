{-# LANGUAGE FlexibleContexts #-}

module Choke
    (
    ) where


import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S

import Peer

data PeerHandlerMessage = FromChokeManager



type PeerChain = [PeerRecord]

data PeerRecord = PeerRecord
    { pPeerId   :: PeerId
    , pInfoHash :: InfoHash
    , pChannel  :: TChan PeerHandlerMessage
    }

instance Show PeerRecord where
    show (PeerRecord peerId _ _) = "(Peer " ++ show peerId ++ ")"

data PeerRate = PeerRate
    { pUpRate   :: Double
    , pDownRate :: Double
    } deriving (Show)

data PeerState = PeerState
    { pChokingUs      :: Bool
    , pInterestedInUs :: Bool
    , pSnubsUs        :: Bool
    , pIsSeeder       :: Bool
    } deriving (Show)

type RateMap = M.Map PeerId (PeerRate, PeerState)


data ChokeState = ChokeState
    { _chain      :: PeerChain      -- ^ The order in which peers are optimistically unchoked
    , _rateMap    :: RateMap        -- ^ Map from Peer ThreadIds to state
    , _seeding    :: S.Set InfoHash -- ^ Set of torrents we seed
    , _upslots    :: Int            -- ^ Current number of upload slots
    , _chokeRound :: Int            -- ^ Counted down by one from 2. If 0 then we should
    }


chokeRound :: (MonadState ChokeState m) => m ()
chokeRound = do
    round <- gets _chokeRound
    if (round == 0)
        then do
            advanceChain
            modify $ \st -> st { _chokeRound = 2 }
        else
            modify $ \st -> st { _chokeRound = _chokeRound st - 1 }
    rechoke


advanceChain :: (MonadState ChokeState m) => m ()
advanceChain = do
    chain   <- gets _chain
    rateMap <- gets _rateMap
    let (front, back) = break (breakPoint rateMap) chain
    modify $ \st -> st { _chain = back ++ front }
  where
    breakPoint rateMap peer =
        case M.lookup (pPeerId peer) rateMap of
            Nothing      -> True -- new peer, give it chance
            Just (_, ps) -> pInterestedInUs ps && not (pChokingUs ps)


rechoke :: (MonadState ChokeState m) => m ()
rechoke = do
    chain   <- gets _chain
    upslots <- gets _upslots
    seeding <- gets _seeding
    rateMap <- gets _rateMap

    let (seed, down) = splitIntoSeedersAndLeechers seeding rateMap chain

    electedPeers <- selectPeers upslots down seed
    performChokingUnchoking electedPeers chain


splitIntoSeedersAndLeechers :: S.Set InfoHash -> RateMap -> [Peer] -> ([Peer], [Peer])
splitIntoSeedersAndLeechers seeding rateMap chain =
    foldl' splitter ([], []) chain
  where
    splitter (seeder, leecher) peer =
        case M.lookup (pPeerId peer) rateMap of
            Nothing                        -> (seeders, leechers)
            Just (_, ps)
                | not (pInterestedInUs ps) -> (seeders, leechers)
                | S.member (pInfoHash peer) seeding
                                           -> (seeders, peer : leechers)
                | otherwise                -> (peer : seeders, leechers)

selectPeers = undefined
performChokingUnchoking = undefined


