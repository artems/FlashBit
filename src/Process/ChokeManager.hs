module Process.ChokeManager
    ( runChokeManager
    , ChokeManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)

import Data.List (partition, foldl', sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T

import System.Random

import Timer
import Torrent
import Process
import Process.Channel


data ChokeManagerMessage
    = ChokeMTick
      -- ^ Request that we run another round
    | ChokeMAddPeer InfoHash ThreadId (TChan PeerHandlerMessage)
      -- ^ Request that this peer is added
    | ChokeMRemovePeer ThreadId
      -- ^ Request that this peer is removed
    | ChokeMPieceDone InfoHash PieceNum
      -- ^ Note that a given piece is done
    | ChokeMBlockComplete InfoHash PieceNum PieceBlock
      -- ^ Note that a block is complete (endgame)
    | ChokeMTorrentComplete InfoHash
      -- ^ Note that the torrent in question is complete


data PConf = PConf
    { _chokeMChan :: TChan ChokeManagerMessage
    , _rateV      :: RateTVar
    }

instance ProcessName PConf where
    processName _ = "ChokeManager"


data PState = PState
    { _chokeRound  :: Int      -- ^ Counted down by one from 2. If 0 then we should
                               --   advance the peer chain. (Optimistic Unchoking)
    , _uploadSlots :: Int      -- ^ Current number of upload slots
    , _seeding     :: S.Set InfoHash -- ^ Set of torrents we seed
    , _rateMap     :: ChokeMRateMap  -- ^ Map from Peer ThreadIds to state
    , _chain       :: PeerChain      -- ^ The order in which peers are optimistically unchoked
    }


chokeRound :: Int
chokeRound = 30


defaultOptimisticSlots :: Int
defaultOptimisticSlots = 2


runChokeManager :: RateTVar -> TChan ChokeManagerMessage -> IO ()
runChokeManager rateV chokeMChan = do
    let slots  = calcUploadSlots 100 Nothing
    let pconf  = PConf chokeMChan rateV
        pstate = PState 2 slots S.empty M.empty []

    setTimeout chokeRound $
        atomically $ writeTChan chokeMChan ChokeMTick

    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState ChokeManagerMessage
wait = do
    chokeChan <- asks _chokeMChan
    liftIO . atomically $ readTChan chokeChan


receive :: ChokeManagerMessage -> Process PConf PState ()
receive message = do
    case message of
        ChokeMTick -> do
            debugP "Тик"
            chokeMChan <- asks _chokeMChan
            liftIO $ setTimeout chokeRound $
                atomically $ writeTChan chokeMChan ChokeMTick
            updateDB
            runRechokeRound

        ChokeMAddPeer infohash threadId peerChan -> do
            debugP $ "Добавляем пир " ++ show (infohash, threadId)
            addPeer peerChan infohash threadId

        ChokeMRemovePeer threadId -> do
            debugP $ "Удаляем пир " ++ show threadId
            modify $ \st -> st
                { _chain   = filter (not . isPeer threadId) (_chain st)
                , _rateMap = M.delete threadId (_rateMap st)
                }

        ChokeMBlockComplete infohash pieceNum block ->
            informBlockComplete infohash pieceNum block

        ChokeMPieceDone infohash pieceNum ->
            informDone infohash pieceNum

        ChokeMTorrentComplete infohash ->
            modify $ \st -> st { _seeding = S.insert infohash $ _seeding st }


  where
    isPeer threadId peer
        | threadId == _peerThreadId peer = True
        | otherwise                      = False


type PeerChain = [PeerThread]

data PeerThread = PeerThread
    { _peerThreadId :: ThreadId
    , _peerInfoHash :: InfoHash
    , _peerChan     :: TChan PeerHandlerMessage
    }

instance Show PeerThread where
    show (PeerThread threadId _ _) = "(Peer " ++ show threadId ++ "...)"


data ChokeMPeerRate  = ChokeMPeerRate
    { _chokePeerUpRate   :: Double
    , _chokePeerDownRate :: Double
    } deriving (Show)


data ChokeMPeerState = ChokeMPeerState
    { _chokePeerChokingUs      :: Bool -- ^ True if the peer is choking us
    , _chokePeerInterestedInUs :: Bool -- ^ Reflection from Peer DB
    , _chokePeerIsASeeder      :: Bool -- ^ True if the peer is a seeder
    , _chokePeerIsSnubbed      :: Bool -- ^ True if peer snubs us
    } deriving (Show)


type ChokeMRateMap = M.Map ThreadId (ChokeMPeerRate, ChokeMPeerState)


updateDB :: Process PConf PState ()
updateDB = do
    rateV <- asks _rateV
    rateUpdate <- liftIO . atomically $ do
        rate <- readTVar rateV
        writeTVar rateV []
        return rate

    case rateUpdate of
        []      -> return ()
        updates -> do
            debugP $ "Обновление скоростей с момента последнего обновления: " ++ show updates
            modify $ \st -> st { _rateMap = foldl f (_rateMap st) $ reverse updates }
  where
    f rateMap (threadId, peerRate) =
        M.insert threadId (updatePeerRate peerRate, updatePeerState peerRate) rateMap
    updatePeerRate peerRate = ChokeMPeerRate
        { _chokePeerUpRate   = _peerUpRate peerRate
        , _chokePeerDownRate = _peerDownRate peerRate
        }
    updatePeerState peerRate = ChokeMPeerState
        { _chokePeerInterestedInUs = _peerInterested peerRate
        , _chokePeerIsASeeder      = _peerSeeding peerRate
        , _chokePeerChokingUs      = _peerChokingUs peerRate
        , _chokePeerIsSnubbed      = _peerSnubs peerRate
        }


addPeer :: TChan PeerHandlerMessage -> InfoHash -> ThreadId -> Process PConf PState ()
addPeer peerChan infohash threadId = do
    chain <- gets _chain
    point <- liftIO $ getStdRandom $ \gen -> randomR (0, length chain - 1) gen
    let (front, back) = splitAt point chain
    modify $ \st -> st { _chain = front ++ newPeer : back }
  where
    newPeer = PeerThread threadId infohash peerChan


runRechokeRound :: Process PConf PState ()
runRechokeRound = do
    round <- gets _chokeRound
    if (round == 0)
        then do
            advancePeerChain
            modify $ \st -> st { _chokeRound = 2 }
        else
            modify $ \st -> st { _chokeRound = _chokeRound st - 1 }
    rechoke


advancePeerChain :: Process PConf PState ()
advancePeerChain = do
    chain   <- gets _chain
    rateMap <- gets _rateMap
    let (front, back) = break (breakPoint rateMap) chain
    modify $ \st -> st { _chain = back ++ front }
  where
    breakPoint rateMap peer =
        case M.lookup (_peerThreadId peer) rateMap of
            Nothing     -> True
            Just (_, p) -> _chokePeerInterestedInUs p && _chokePeerChokingUs p


rechoke :: Process PConf PState ()
rechoke = do
    uploadSlots <- gets _uploadSlots
    chain       <- gets _chain
    seeding     <- gets _seeding
    rateMap     <- gets _rateMap
    let (seed, down) = splitSeedLeech seeding rateMap chain
    debugP $ "Пиры:  "     ++ show (map _peerThreadId chain)
    debugP $ "Скорости:  " ++ show rateMap
    debugP $ "Сидеры: "    ++ show seed
    debugP $ "Личеры: "    ++ show down
    electedPeers <- selectPeers uploadSlots down seed
    performChokingUnchoking electedPeers chain


splitSeedLeech :: S.Set InfoHash -> ChokeMRateMap -> [PeerThread] -> ([PeerThread], [PeerThread])
splitSeedLeech seeding rateMap chain = foldl' splitter ([], []) chain
  where
    splitter (seeds, leeching) p =
        case M.lookup (_peerThreadId p) rateMap of
            Nothing -> (seeds, leeching) -- Know nothing on the peer yet
            Just (_, st) | _chokePeerIsASeeder st             -> (seeds, leeching)
                         | not (_chokePeerInterestedInUs st)  -> (seeds, leeching)
                         | S.member (_peerInfoHash p) seeding -> (p : seeds, leeching)
                         | _chokePeerIsSnubbed st             -> (seeds, leeching)
                         | otherwise                          -> (seeds, p : leeching)


compareInv :: Ord a => a -> a -> Ordering
compareInv x y =
    case compare x y of
        LT -> GT
        EQ -> EQ
        GT -> LT

comparingWith :: Ord a => (a -> a -> Ordering) -> (b -> a) -> b -> b -> Ordering
comparingWith comp project x y =
    comp (project x) (project y)

sortLeech
    :: [(PeerThread, (ChokeMPeerRate, ChokeMPeerState))]
    -> [(PeerThread, (ChokeMPeerRate, ChokeMPeerState))]
sortLeech = sortBy $ comparingWith compareInv (_chokePeerDownRate . fst . snd)

sortSeeds
    :: [(PeerThread, (ChokeMPeerRate, ChokeMPeerState))]
    -> [(PeerThread, (ChokeMPeerRate, ChokeMPeerState))]
sortSeeds = sortBy $ comparingWith compareInv (_chokePeerUpRate . fst . snd)


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


assignUploadSlots :: Int -> Int -> Int -> (Int, Int)
assignUploadSlots slots numDownPeers numSeedPeers =
    shuffleSeeders . shuffleDownloaders $ (downloaderSlots, seederSlots)
  where
    slotRound :: Double -> Double -> Int
    slotRound ss fraction = max 1 $ round $ ss * fraction

    seederSlots     = slotRound (fromIntegral slots) 0.3
    downloaderSlots = slotRound (fromIntegral slots) 0.7

    shuffleDownloaders (dSlots, sSlots) =
        case max 0 (dSlots - numDownPeers) of
          0 -> (dSlots, sSlots)
          k -> (dSlots - k, sSlots + k)

    shuffleSeeders (dSlots, sSlots) =
        case max 0 (sSlots - numSeedPeers) of
          0 -> (dSlots, sSlots)
          k -> (min (dSlots + k) numDownPeers, sSlots - k)


selectPeers :: Int -> [PeerThread] -> [PeerThread] -> Process PConf PState (S.Set ThreadId)
selectPeers uploadSlots downPeers seedPeers = do
    rateMap <- gets _rateMap
    let selector p = maybe (p, (ChokeMPeerRate 0.0 0.0, ChokeMPeerState True False False False))
                           (\a -> (p, a))
                           (M.lookup (_peerThreadId p) rateMap)
        dp = map selector downPeers
        sp = map selector seedPeers
        (nDownSlots, nSeedSlots) = assignUploadSlots uploadSlots (length downPeers) (length seedPeers)
        downPids = S.fromList $ map (_peerThreadId . fst) $ take nDownSlots $ sortLeech dp
        seedPids = S.fromList $ map (_peerThreadId . fst) $ take nSeedSlots $ sortSeeds sp
    debugP $ "Личеры: " ++ show (length downPeers) ++ ", Сидеры: " ++ show (length seedPeers)
    debugP $ "Слоты: " ++ show nDownSlots ++ " downloads, " ++ show nSeedSlots ++ " seeders"
    debugP $ "Выбранные пиры личеры: " ++ show downPids ++ "; сидеры: " ++ show seedPids
    return $ assertSlots (nDownSlots + nSeedSlots) (S.union downPids seedPids)
  where
    assertSlots slots = assert (uploadSlots >= slots)


msgPeer :: TChan PeerHandlerMessage -> PeerChokeMessage -> Process PConf PState ()
msgPeer peerChan = liftIO . atomically . writeTChan peerChan . FromChokeManager


performChokingUnchoking :: S.Set ThreadId -> [PeerThread] -> Process PConf PState ()
performChokingUnchoking elected chain = do
    rateMap <- gets _rateMap
    _ <- T.mapM unchoke electedPeers
    optChoke rateMap defaultOptimisticSlots nonElectedPeers
  where
    (electedPeers, nonElectedPeers) = partition (\p -> S.member (_peerThreadId p) elected) chain
    choke p = do
        debugP $ "Choking: " ++ show p
        msgPeer (_peerChan p) ChokePeer
    unchoke p = do
        debugP $ "Unchoking: " ++ show p
        msgPeer (_peerChan p) UnchokePeer

    optChoke _       _ []       = return ()
    optChoke rateMap 0 (p : ps) =
        case M.lookup (_peerThreadId p) rateMap of
            Nothing ->
                choke p >> optChoke rateMap 0 ps
            Just (_, st) ->
                if _chokePeerInterestedInUs st
                   then choke p   >> optChoke rateMap 0 ps
                   else unchoke p >> optChoke rateMap 0 ps
    optChoke rateMap k (p : ps) =
        case M.lookup (_peerThreadId p) rateMap of
            Nothing ->
                unchoke p >> optChoke rateMap (k - 1) ps
            Just (_, st) ->
                if _chokePeerInterestedInUs st
                   then unchoke p >> optChoke rateMap (k - 1) ps
                   else unchoke p >> optChoke rateMap k ps


informDone :: InfoHash -> PieceNum -> Process PConf PState ()
informDone infohash pieceNum = do
    chain <- gets _chain
    T.mapM inform chain
    return ()
 where
    inform p | (_peerInfoHash p) == infohash = sendDone p
             | otherwise                     = return ()
    sendDone p = msgPeer (_peerChan p) (PieceCompleted pieceNum)


informBlockComplete :: InfoHash -> PieceNum -> PieceBlock -> Process PConf PState ()
informBlockComplete infohash pieceNum block = do
    chain <- gets _chain
    T.mapM inform chain
    return ()
  where
    inform p
        | (_peerInfoHash p) == infohash = sendComp p
        | otherwise                     = return ()
    sendComp p = msgPeer (_peerChan p) (CancelBlock pieceNum block)


