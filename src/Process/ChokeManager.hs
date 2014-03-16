module Process.ChokeManager
    ( ChokeManagerMessage(..)
    , PeerRateInfo(..)
    -- * Interface
    , runChockManager
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable as T

import Prelude hiding (catch, log)

import System.Random

import Channels hiding (Peer)
import Process
import Process.Timer
import Supervisor
import Torrent hiding (infoHash)

-- | Messages to the Choke Manager
data ChokeMgrMsg
    = Tick
      -- ^ Request that we run another round
    | RemovePeer
      -- ^ Request that this peer is removed
    | AddPeer PeerId InfoHash PeerChannel
      -- ^ Request that this peer is added
    | PieceDone InfoHash PieceNum
      -- ^ Note that a given piece is done
    | BlockComplete InfoHash PieceNum Block
      -- ^ Note that a block is complete (endgame)
    | TorrentComplete InfoHash
      -- ^ Note that the torrent in question is complete


data PeerRateInfo = PeerRateInfo
    { peerUpRate     :: Double
    , peerDownRate   :: Double
    , peerChokeUs    :: Bool
    , peerInterested :: Bool
    , peerSeeding    :: Bool
    , peerSnubUs     :: Bool
    } deriving (Show)

type RateTVar = TVar [PeerRateInfo]

data PConf = PConf
    { cChockChan :: TChan ChokeManagerMessage
    , cRateTV    :: RateTVar
    }

instance ProcessName PConf where
  processName _ = "ChokeManager"


roundTickSecs :: Int
roundTickSecs = 10

runChokeManager :: TChan ChokeManagerMessage -> RateTVar -> Int -> IO Reason
runChokeManager chokeChan rateTV slots = do
    setTimeout rountTickSecs $ atomically $ writeTChan chokeChan Tick
    let pconf = PConf chokeChan rateTV
        pstate = PState (initPeerDB $ calcUploadSlots slots Nothing)
    process0 "ChokeManager" conf state wait receive
  where
    initPeerDB slots = PeerDB 2 slots S.empty M.empty []


wait :: Process PConf PState ChokeManagerMessage
wait = do
    chokeChan <- asks cChokeChan
    liftIO . atomically $ readTChan chokeChan


receive :: ChokeManagerMessage -> Process PConf PState ()
receive message =
    case message of
        Tick ->
            tick

        RemovePeer ->
            removePeer

        AddPeer peerId infoHash peerChan -> do
            debugP $ "Adding peer " ++ show infoHash
            addPeer infoHash peerChan

       BlockComplete infoHash pieceNum block ->
            informBlockComplete infoHash pieceNum block

       PieceDone infoHash pieceNum ->
            informDone infoHash pieceNum

       TorrentComplete infoHash ->
            modify $ \s -> s { seeding = S.insert infoHash $ seeding s }


tick = do
    debugP "Tick"
    chokeChan <- asks cChokeChan
    setTimeout rountTickSecs $ atomically $ writeTChan chokeChan Tick
    updateDB
    runRechokeRound

removePeer peerId = do
    debugP $ "Removing peer " ++ show peerId
    modify $ \s -> s
                { chain = filter (not . isPeer peerId) (chain s)
                , rateMap = M.delete tid (rateMap db)
                }
  where
    isPeer peerId peer
        | peerId == pPeerId peer = True
        | otherwise = False



-- The data structure is split into pieces so it is easier to manipulate.
-- The PeerDB is the state we thread around in the process. The PChain contains all
-- the important information about processes.
type PChain = [Peer]

-- | Main data for a peer
data Peer = Peer
        { pPeerId   :: String
        , pInfoHash :: InfoHash
        , pChannel  :: PeerChannel
        }

instance Show Peer where
    show (Peer pid _ _) = "(Peer " ++ show pid ++ "...)"

-- | Peer upload and download ratio
data PeerRate  = PeerRate
    { pUpRate   :: Double
    , pDownRate :: Double
    } deriving (Show)

-- | Current State of the peer
data PeerState = PeerState
    { pChokingUs :: Bool -- ^ True if the peer is choking us
    , pInterestedInUs :: Bool -- ^ Reflection from Peer DB
    , pIsASeeder :: Bool -- ^ True if the peer is a seeder
    , pIsSnubbed :: Bool -- ^ True if peer snubs us
    } deriving (Show)

type RateMap = M.Map PeerId (PeerRate, PeerState)

data PeerDB = PeerDB
    { chokeRound  :: Int      -- ^ Counted down by one from 2. If 0 then we should
                              --   advance the peer chain. (Optimistic Unchoking)
    , uploadSlots :: Int      -- ^ Current number of upload slots
    , seeding     :: S.Set InfoHash -- ^ Set of torrents we seed
    , rateMap     :: RateMap  -- ^ Map from Peer ThreadIds to state
    , chain       :: PChain   -- ^ The order in which peers are optimistically unchoked
    }

-- | Update the Peer Database with the newest information from peers
updateDB :: Process PConf PState ()
updateDB = do
    rateTV <- asks cRateTV
    rateUpdate <- liftIO . atomically $ do
        rate <- readTVar rateTV
        writeTVar rc []
        return rate

    case rateUpdate of
        [] -> return ()
        updates ->  do
            debugP $ "Rate updates since last round: " ++ show updates
            let peerRate' = PeerRate
                    { pUpRate = peerUpRate rateInfo
                    , pDownRate = peerDownRate rateInfo
                    }
                peerState' = PeerState
                    { pChokingUs      = peerChokingUs rateInfo
                    , pInterestedInUs = peerInterested rateInfo
                    , pIsASeeder      = peerSeeding rateInfo
                    , pIsSnubbed      = peerSnubs rateInfo
                    }
            let f old (peerId, peerRateInfo) =
                    M.insert peerId (peerRate' peerRateInfo, peerState' peerRateInfo) old
            let nm m = foldl f m $ reverse updates
            modify $ \s -> s { rateMap = nm (rateMap s) }


addPeer :: PeerId -> InfoHash -> TChan PeerMessage -> Process PConf PState ()
addPeer peerId infoHash peerChan = do
    chain <- gets sChain
    point <- liftIO $ getStdRandom $ \gen -> randomR (0, length chain - 1) gen)
    debugP $ "split point " ++ show point
    let (front, back) = splitAt point chain
    modify $ \db -> db { sChain = (front ++ initPeer : back) }
  where
    initPeer = Peer peerId infoHash peerChan


runRechokeRound :: Process PConf PState ()
runRechokeRound = do
    cRound <- gets sChokeRound
    if (cRound == 0)
        then do
            advancePeerChain
            modify $ \st -> st { sChokeRound = 2 }
        else
            modify $ \st -> st { sChokeRound = sChokeRound st - 1 }
    rechoke


-- | Advance the peer chain to the next peer eligible for optimistic
--   unchoking. That is, skip peers which are not interested in our pieces
--   and peers which are not choking us. The former we can't send any data to,
--   so we can't get better speeds at them. The latter are already sending us data,
--   so we know how good they are as peers.
advancePeerChain :: Process PConf PState ()
advancePeerChain = do
    chain   <- gets sChain
    rateMap <- gets sRateMap
    let (front, back) = break (breakPoint rateMap) chain
    modify $ \st -> st { sChain = back ++ front }
  where
    breakPoint rateMap peer =
        case M.lookup (pPeerId peer) rateMap of
            Just (_, p') -> pInterestedInUs p' && pChokingUs p'
            Nothing      -> True -- really new peer, give it the chance


rechoke :: Process PConf PState ()
rechoke = do
    upSlots <- gets sUploadSlots
    chain   <- gets sChain
    sd      <- gets sSeeding
    rateMap <- gets rateMap
    debugP $ "Chain is:  " ++ show (map peerId chain)
    debugP $ "RateMap is:  " ++ show rateMap
    let (seed, down) = splitSeedLeech sd rateMap chain
    debugP $ "Seeders " ++ show seed
    debugP $ "Downloaders " ++ show down
    electedPeers <- selectPeers upSlots down seed
    performChokingUnchoking electedPeers chain


-- | Function to split peers into those where we are seeding and those where we
--   are leeching.  also prunes the list for peers which are not interesting.
splitSeedLeech :: S.Set InfoHash -> RateMap -> [Peer] -> ([Peer], [Peer])
splitSeedLeech seeders rateMap ps = foldl' splitter ([], []) ps
  where
    splitter (seeds, leeching) p =
        case M.lookup (peerId p) rateMap of
            Nothing -> (seeds, leeching) -- Know nothing on the peer yet
            Just (_, pr)
                | pIsASeeder pr || not (pInterestedInUs pr) -> (seeds, leeching)
                | S.member (pInfoHash p) seeders            -> (p : seeds, leeching)
                | pIsSnubbed pr                             -> (seeds, leeching)
                | otherwise                                 -> (seeds, p : leeching)

-- | Comparison with inverse ordering
compareInv :: Ord a => a -> a -> Ordering
compareInv x y =
    case compare x y of
        LT -> GT
        EQ -> EQ
        GT -> LT

comparingWith :: Ord a => (a -> a -> Ordering) -> (b -> a) -> b -> b -> Ordering
comparingWith comp project x y =
    comp (project x) (project y)

-- | Leechers are sorted by their current download rate. We want to keep fast peers around.
sortLeech :: [(Peer, (PeerRate, PeerState))] -> [(Peer, (PeerRate, PState))]
sortLeech = sortBy $ comparingWith compareInv (pDownRate . fst . snd)

-- | Seeders are sorted by their current upload rate.
sortSeeds :: [(Peer, (PRate, PState))] -> [(Peer, (PRate, PState))]
sortSeeds = sortBy $ comparingWith compareInv (pUpRate . fst . snd)


-- | Calculate the amount of upload slots we have available. If the
--   number of slots is explicitly given, use that. Otherwise we
--   choose the slots based the current upload rate set. The faster
--   the rate, the more slots we allow.
calcUploadSlots :: Int -> Maybe Int -> Int
calcUploadSlots _ (Just n) = n
calcUploadSlots rate Nothing
    | rate <= 0 = 7 -- This is just a guess
    | rate <  9 = 2
    | rate < 15 = 3
    | rate < 42 = 4
    | otherwise = calcRate $ fromIntegral rate
  where
    calcRate :: Double -> Int
    calcRate x = round $ sqrt (x * 0.6)


-- | The call @assignUploadSlots c ds ss@ will assume that we have @c@
--   slots for uploading at our disposal. The list @ds@ will be peers
--   that we would like to upload to among the torrents we are
--   currently downloading. The list @ss@ is the same thing but for
--   torrents that we seed. The function returns a pair @(kd,ks)@
--   where @kd@ is the number of downloader slots and @ks@ is the
--   number of seeder slots.
--
--   The function will move surplus slots around so all of them gets used.
assignUploadSlots :: Int -> Int -> Int -> (Int, Int)
assignUploadSlots slots numDownPeers numSeedPeers =
    -- Shuffle surplus slots around so all gets used
    shuffleSeeders . shuffleDownloaders $ (downloaderSlots, seederSlots)
  where
    -- Calculate the slots available for the downloaders and seeders
    --   We allocate 70% of them to leeching and 30% of the to seeding
    --   though we assign at least one slot to both
    slotRound :: Double -> Double -> Int
    slotRound ss fraction = max 1 $ round $ ss * fraction

    downloaderSlots = slotRound (fromIntegral slots) 0.7
    seederSlots     = slotRound (fromIntegral slots) 0.3

    -- If there is a surplus of downloader slots, then assign them to
    --  the seeder slots
    shuffleDownloaders (dSlots, sSlots) =
        case max 0 (dSlots - numDownPeers) of
          0 -> (dSlots, sSlots)
          k -> (dSlots - k, sSlots + k)

    -- If there is a surplus of seeder slots, then assign these to
    --   the downloader slots. Limit the downloader slots to the number
    --   of downloaders, however
    shuffleSeeders (dSlots, sSlots) =
        case max 0 (sSlots - numSeedPeers) of
          0 -> (dSlots, sSlots)
          k -> (min (dSlots + k) numDownPeers, sSlots - k)


-- | @selectPeers upSlots d s@ selects peers from a list of downloader peers @d@ and a list of seeder
--   peers @s@. The value of @upSlots@ defines the number of upload slots available
selectPeers :: Int -> [Peer] -> [Peer] -> Process PConf PState (S.Set PeerId)
selectPeers ups downPeers seedPeers = do
    rateMap <- gets rateMap
    let selector p = maybe
                        (p, (PRate 0.0 0.0, PState True False False False))
                        (p,) (M.lookup (pPeerId p) rateMap)
        dp = map selector downPeers
        sp = map selector seedPeers
        (nDownSlots, nSeedSlots) = assignUploadSlots ups (length downPeers) (length seedPeers)
        downPids = S.fromList $ map (pPeerId . fst) $ take nDownSlots $ sortLeech dp
        seedPids = S.fromList $ map (pPeerId . fst) $ take nSeedSlots $ sortSeeds sp
    debugP $ "Slots: " ++ show nDownSlots ++ " downloads, " ++ show nSeedSlots ++ " seeders"
    debugP $ "Leechers: " ++ show (length downPeers) ++ ", Seeders: " ++ show (length seedPeers)
    debugP $ "Electing peers - leechers: " ++ show downPids ++ "; seeders: " ++ show seedPids
    return (S.union downPids seedPids)

-- | Send a message to the peer process at PeerChannel. Message is sent asynchronously
--   to the peer in question. If the system is really loaded, this might
--   actually fail since the order in which messages arrive might be inverted.
msgPeer :: PeerChannel -> PeerChokeMsg -> Process PConf PState ()
msgPeer ch = liftIO . atomically . writeTChan ch . FromChokeMgr

-- | This function performs the choking and unchoking of peers in a round.
performChokingUnchoking :: S.Set PeerId -> [Peer] -> Process PConf PState ()
performChokingUnchoking elected peers = do
    _ <- T.mapM unchoke electedPeers
    rateMap <- gets sRateMap
    optChoke rateMap defaultOptimisticSlots nonElectedPeers
  where
    -- Partition the peers in elected and non-elected
    (electedPeers, nonElectedPeers) = partition (\rd -> S.member (peerId rd) elected) peers
    choke p = do
        debugP $ "Choking: " ++ show p
        msgPeer (pChannel p) ChokePeer
    unchoke p = do
        debugP $ "Unchoking: " ++ show p
        msgPeer (pChannel p) UnchokePeer

    -- If we have k optimistic slots, @optChoke k peers@ will unchoke the first
    -- @k@ peers interested in us. The rest will either be unchoked if they are
    -- not interested (ensuring fast start should they become interested); or
    -- they will be choked to avoid TCP/IP congestion.
    optChoke _rm     _ []       = return ()
    optChoke rateMap 0 (p : ps) =
        case M.lookup (pPeerId p) rateMap of
            Nothing ->
                choke p >> optChoke rateMap 0 ps
            Just (_, st) ->
                if pInterestedInUs st
                   then choke p >> optChoke rateMap 0 ps
                   else unchoke p >> optChoke rateMap 0 ps
    optChoke rateMap k (p : ps) =
        case M.lookup (pPeerId p) rateMap of
            Nothing ->
                unchoke p >> optChoke rm (k - 1) ps
            Just (_, st) ->
                if pInterestedInUs st
                   then unchoke p >> optChoke rateMap (k - 1) ps
                   else unchoke p >> optChoke rateMap k ps

informDone :: InfoHash -> PieceNum -> ChokeMgrProcess ()
informDone ih pn = do
    chn <- gets chain
    T.mapM inform chn >> return ()
 where inform p | (pInfoHash p) == ih = sendDone p >> return ()
                | otherwise           = return ()
       sendDone p = msgPeer (pChannel p) (PieceCompleted pn)

informBlockComplete :: InfoHash -> PieceNum -> Block -> Process PConf PState ()
informBlockComplete infoHash pieceNum block = do
    chain <- gets sChain
    T.mapM inform chain >> return ()
  where
    inform p | (pInfoHash p) == infoHash = sendComplete p >> return ()
             | otherwise                 = return ()
    sendComplete p = msgPeer (pChannel p) (CancelBlock pieceNum block)


