module Process.Status
    ( runStatus
    , UpDownStat(..)
    , StatusState(..)
    , StatusMessage(..)
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M


import Process
import Torrent
import Protocol
import Platform.Process
import Timer


data PConf  = PConf
    { cStatusTVar :: TVar [UpDownStat]
    , cStatusChan :: TChan StatusMessage
    }

type PState = M.Map InfoHash StatusState

data StatusState = StatusState
    { sLeft         :: Integer
    , sUploaded     :: Integer
    , sDownloaded   :: Integer
    , sComplete     :: Maybe Integer
    , sIncomplete   :: Maybe Integer
    , sStatus       :: TorrentState
    }


data StatusMessage
    = TrackerStat
        { trackInfoHash :: InfoHash
        , trackComplete :: Maybe Integer
        , trackIncomplete :: Maybe Integer
        }
    | InsertTorrent InfoHash Integer
    | RemoveTorrent InfoHash
    | CompletedPiece InfoHash Integer
    | TorrentCompleted InfoHash
    | RequestStatus InfoHash (TMVar StatusState)
    | RequestAllTorrents (TMVar [(InfoHash, StatusState)])
    | TorrentExists InfoHash (TMVar Bool)
    | StatusTick
    | Terminate


data UpDownStat = UpDownStat
    { udInfoHash :: InfoHash
    , udUploaded :: Integer
    , udDownloaded :: Integer
    }


instance Show StatusState where
    show (StatusState left up down compl incompl status) = concat
        [ "{ Left:       " ++ show left    ++ "\n"
        , "  Uploaded:   " ++ show up      ++ "\n"
        , "  Downloaded: " ++ show down    ++ "\n"
        , "  Complete:   " ++ show compl   ++ "\n"
        , "  Incomplete: " ++ show incompl ++ "\n"
        , "  Status:     " ++ show status  ++ " }"
        ]


mkStatusState :: Integer -> StatusState
mkStatusState left = StatusState
    { sLeft = left
    , sUploaded = 0
    , sDownloaded = 0
    , sComplete = Nothing
    , sIncomplete = Nothing
    , sStatus = if left == 0 then Seeding else Leeching
    }


runStatus :: TVar [UpDownStat] -> TChan StatusMessage -> IO Reason
runStatus statusTV statusChan = process0 "Status" conf M.empty wait receive
  where
    conf = PConf statusTV statusChan
    wait = liftIO . atomically $ readTChan statusChan
    receive msg = handleMessage msg


handleMessage :: StatusMessage -> Process PConf PState ()
handleMessage msg =
    case msg of
        TrackerStat infoHash incoml compl ->
            modify $ \m ->
                M.adjust (\s -> s { sIncomplete = incoml, sComplete = compl }) infoHash m

        CompletedPiece infoHash bytes ->
            modify $ \m ->
                M.adjust (\s -> s { sLeft = (sLeft s) - bytes }) infoHash m

        InsertTorrent infoHash left ->
            modify $ \m -> M.insert infoHash (mkStatusState left) m

        RemoveTorrent infoHash ->
            modify $ \m -> M.delete infoHash m

        RequestStatus infoHash v -> do
            m <- get
            case M.lookup infoHash m of
                Just s  -> liftIO . atomically $ putTMVar v s
                Nothing -> fail "RequestStatus: unknown info_hash"

        RequestAllTorrents v -> do
            m <- get
            liftIO . atomically $ putTMVar v (M.toList m)

        TorrentExists infoHash v -> do
            m <- get
            liftIO . atomically $ putTMVar v (M.member infoHash m)

        TorrentCompleted infoHash -> do
            m <- get
            s <- case M.lookup infoHash m of
                Just s  -> return s
                Nothing -> fail "TorrentCompleted: unknown torrent"
            put $ M.insert infoHash (s { sStatus = Seeding }) m

        StatusTick -> do
            fetchUpdates
            chan <- asks cStatusChan
            _ <- liftIO . setTimeout 5 . atomically $ writeTChan chan StatusTick
            return ()

        Terminate ->
            stopProcess Shutdown


fetchUpdates :: Process PConf PState ()
fetchUpdates = do
    statusTV <- asks cStatusTVar
    updates <- liftIO . atomically $ do
        updates <- readTVar statusTV
        writeTVar statusTV []
        return updates
    mapM_ f updates
  where
    f (UpDownStat infoHash up down) =
        modify $ \m ->
            M.adjust (\s -> s
                { sUploaded = (sUploaded s) + up
                , sDownloaded = (sDownloaded s) + down
                }
            ) infoHash m


