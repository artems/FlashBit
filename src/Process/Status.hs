module Process.Status
    ( StatusMessage(..)
    , UpDownStat(..)
    , runStatus
    ) where


import Control.Concurrent.STM

import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify, get)
import Control.Monad.Reader (asks)

import qualified Data.Map as M

import Torrent

import Timer
import Process
import Process.Channel


data UpDownStat = UpDownStat
    { _statInfoHash   :: InfoHash
    , _statUploaded   :: Integer
    , _statDownloaded :: Integer
    }


data PConf = PConf
    { _statusV    :: TVar [UpDownStat]
    , _statusChan :: TChan StatusMessage
    }

instance ProcessName PConf where
    processName _ = "Status"


type PState = M.Map InfoHash StatusState


instance Show StatusState where
    show (StatusState up down left complete incomplete state _) = concat
        [ "{ uploaded:   " ++ show up         ++ "\n"
        , "  downloaded: " ++ show down       ++ "\n"
        , "  left:       " ++ show left       ++ "\n"
        , "  state:      " ++ show state      ++ "\n"
        , "  complete:   " ++ show complete   ++ "\n"
        , "  incomplete: " ++ show incomplete ++ " }"
        ]


runStatus :: TVar [UpDownStat] -> TChan StatusMessage -> IO ()
runStatus statusV statusChan = do
    let pconf  = PConf statusV statusChan
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
        StatusTrackerStat infohash complete incomplete -> do
            adjust infohash $ \st -> st { _torrentComplete = complete, _torrentIncomplete = incomplete }

        StatusCompletedPiece infohash bytes -> do
            adjust infohash $ \st -> st { _left = (_left st) - bytes }

        StatusAddTorrent infohash left trackerChan ->
            modify $ M.insert infohash $ mkStatusState left trackerChan

        StatusRemoveTorrent infohash ->
            modify $ M.delete infohash

        StatusExistsTorrent infohash existsV -> do
            m <- get
            liftIO . atomically $ putTMVar existsV $ M.member infohash m

        StatusRequestStatus infohash statusV -> do
            m <- get
            case M.lookup infohash m of
                Just st -> liftIO . atomically $ putTMVar statusV st
                Nothing -> fail $ "unknown info_hash " ++ show infohash

        StatusRequestStatistic statsV -> do
            m <- get
            liftIO . atomically $ putTMVar statsV $ M.toList m

        StatusTorrentCompleted infohash -> do
            m  <- get
            st <- case M.lookup infohash m of
                Just st -> return st
                Nothing -> fail $ "unknown info_hash " ++ show infohash
            liftIO . atomically $ writeTChan (_statusTrackerChan st) TrackerComplete
            modify $ M.insert infohash $ st { _torrentState = Seeding }


adjust :: InfoHash -> (StatusState -> StatusState) -> Process PConf PState ()
adjust infohash f = do
    modify $ \st -> M.adjust f infohash st


mkStatusState :: Integer -> TChan TrackerMessage -> StatusState
mkStatusState left trackerChan
    = StatusState
    { _uploaded   = 0
    , _downloaded = 0
    , _left       = left
    , _torrentComplete   = Nothing
    , _torrentIncomplete = Nothing
    , _torrentState      = if left == 0 then Seeding else Leeching
    , _statusTrackerChan = trackerChan
    }


fetchUpdates :: Process PConf PState ()
fetchUpdates = do
    statusV <- asks _statusV
    updates <- liftIO . atomically $ fetchV statusV
    mapM_ updateStatus updates
  where
    fetchV statusV = do
        update <- readTVar statusV
        writeTVar statusV []
        return update
    updateStatus (UpDownStat infohash up down) =
        adjust infohash $ \st -> st
            { _uploaded   = (_uploaded st)   + up
            , _downloaded = (_downloaded st) + down
            }


