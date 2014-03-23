module Process.Status
    ( runStatus
    , StatusMessage(..)
    , UpDownStat(..)
    ) where


import Control.Concurrent.STM

import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify, get)
import Control.Monad.Reader (asks)

import qualified Data.Map as M

import Torrent

import Timer
import Process


data StatusMessage
    = TrackerStat
        { trackerInfoHash   :: InfoHash
        , trackerComplete   :: Maybe Integer
        , trackerIncomplete :: Maybe Integer
        }
    | CompletedPiece InfoHash Integer
    -- | InsertTorrent InfoHash Integer TrackerChannel
    | RemoveTorrent InfoHash
    | ExistsTorrent InfoHash (TMVar Bool)
    | TorrentCompleted InfoHash
    | RequestStatus InfoHash (TMVar StatusState)
    | RequestStatistic (TMVar [(InfoHash, StatusState)])


data UpDownStat = UpDownStat
    { _statInfoHash   :: InfoHash
    , _statUploaded   :: Integer
    , _statDownloaded :: Integer
    }


data PConf = PConf
    { _statusChan :: TChan StatusMessage
    , _statusV    :: TVar [UpDownStat]
    }

instance ProcessName PConf where
    processName _ = "Status"


data StatusState = StatusState
    { _sUploaded :: Integer
    , _sDownloaded :: Integer
    , _sLeft :: Integer
    , _sComplete :: Maybe Integer
    , _sIncomplete :: Maybe Integer
    , _sState :: TorrentState
    -- , _sTrackerChan :: TrackerChannel
    }

type PState = M.Map InfoHash StatusState


instance Show StatusState where
    show (StatusState up down left complete incomplete state) = concat
        [ "{ uploaded:   " ++ show up         ++ "\n"
        , "  downloaded: " ++ show down       ++ "\n"
        , "  left:       " ++ show left       ++ "\n"
        , "  state:      " ++ show state      ++ "\n"
        , "  complete:   " ++ show complete   ++ "\n"
        , "  incomplete: " ++ show incomplete ++ " }"
        ]


runStatus :: TVar [UpDownStat] -> TChan StatusMessage -> IO ()
runStatus statusV statusChan = do
    let pconf  = PConf statusChan statusV
        pstate = M.empty
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    fetchUpdates
    message <- wait
    receive message
    process


wait :: Process PConf PState StatusMessage
wait = do
    statusChan <- asks _statusChan
    liftIO . atomically $ readTChan statusChan


receive :: StatusMessage -> Process PConf PState ()
receive message =
    case message of
        TrackerStat infohash complete incomplete -> do
            adjust infohash $ \st -> st { _sComplete = complete, _sIncomplete = incomplete }

        CompletedPiece infohash bytes -> do
            adjust infohash $ \st -> st { _sLeft = (_sLeft st) - bytes }

        -- InsertTorrent infohash left trackerChan ->
        --     modify $ M.insert infohash $ mkStatusState left trackerChan

        RemoveTorrent infohash ->
            modify $ M.delete infohash

        ExistsTorrent infohash existsV -> do
            m <- get
            liftIO . atomically $ putTMVar existsV $ M.member infohash m

        RequestStatus infohash statusV -> do
            m <- get
            case M.lookup infohash m of
                Just st -> liftIO . atomically $ putTMVar statusV st
                Nothing -> fail $ "unknown info_hash " ++ show infohash

        RequestStatistic statsV -> do
            m <- get
            liftIO . atomically $ putTMVar statsV $ M.toList m

        TorrentCompleted infohash -> do
            m  <- get
            st <- case M.lookup infohash m of
                Just st -> return st
                Nothing -> fail $ "unknown info_hash " ++ show infohash
            -- liftIO . atomically $ writeTChan (_sTrackerChan st) Complete
            modify $ M.insert infohash $ st { _sState = Seeding }


adjust :: InfoHash -> (StatusState -> StatusState) -> Process PConf PState ()
adjust infohash f = do
    modify $ \st -> M.adjust f infohash st


-- mkStatusState :: Integer -> TrackerChannel -> StatusState
-- mkStatusState left trackerChan =
mkStatusState :: Integer -> StatusState
mkStatusState left =
    StatusState
    { _sUploaded = 0
    , _sDownloaded = 0
    , _sLeft = left
    , _sComplete = Nothing
    , _sIncomplete = Nothing
    , _sState = if left == 0 then Seeding else Leeching
    -- , _sTrackerChan = trackerChan
    }


fetchUpdates :: Process PConf PState ()
fetchUpdates = do
    statusV <- asks _statusV
    updates <- liftIO . atomically $ fetchVar statusV
    mapM_ updateStatus updates
  where
    fetchVar statusV = do
        update <- readTVar statusV
        writeTVar statusV []
        return update
    updateStatus (UpDownStat infohash up down) =
        adjust infohash $ \st -> st
            { _sUploaded = (_sUploaded st) + up
            , _sDownloaded = (_sDownloaded st) + down
            }


