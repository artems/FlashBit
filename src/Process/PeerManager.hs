module Process.PeerManager
    ( runPeerManager
    ) where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

import Network.Socket

import Process
import Protocol
import Platform.Process


data PConf = PConf
    { cPeerEventChan    :: TChan PeerEvent
    , cPeerManagerChan  :: TChan PeerManagerMessage
    }

data PState = PState
    { sPeerId      :: PeerId                 -- ^ Наш peer_id
    , sPeerCount   :: Int                    -- ^ Кол-во активных пиров
    , sPeerQueue   :: [(InfoHash, Peer)]     -- ^ Очередь пиров для подключения
    , sTorrent     :: M.Map InfoHash TorrentLocal -- ^ Список торрентов. см. TorrentLocal
    }

instance ProcessName PConf where
    processName _ = "PeerManager"


-- ^ Эти данные передаются процессу при подключении пира
data TorrentLocal = TorrentLocal
    { pPieceArray :: PieceArray
    }

data PeerManagerMessage
    = NewTorrent InfoHash TorrentLocal
    | StopTorrent InfoHash
    | PeersFromTracker InfoHash [Peer]
    | NewIncoming (Socket, SockAddr)

data PeerEvent
    = Connect InfoHash
    | Disconnect InfoHash


numPeers :: Int
numPeers = 40


runPeerManager :: PeerId -> TChan PeerManagerMessage -> IO Reason
runPeerManager peerId torrentManagerChan = do
    peerEventChan <- newTChanIO
    let conf  = PConf peerEventChan torrentManagerChan
        state = PState peerId 0 [] M.empty
    process0 "PeerManager" conf state wait receive


wait :: Process PConf PState (Either PeerEvent PeerManagerMessage)
wait = do
    peerEventChan <- asks cPeerEventChan
    peerManagerChan <- asks cPeerManagerChan
    liftIO . atomically $ orElse
        (readTChan peerEventChan >>= return . Left)
        (readTChan peerManagerChan >>= return . Right)


receive :: Either PeerEvent PeerManagerMessage -> Process PConf PState ()
receive event = do
    case event of
        Left msg  -> peerEvent msg
        Right msg -> incomingPeers msg
    fillPeers


incomingPeers :: PeerManagerMessage -> Process PConf PState ()
incomingPeers message = case message of
   PeersFromTracker infoHash peers -> do
        debugP "Добавление новых пиров в очередь"
        modify $ \s -> s { sPeerQueue = (map (\p -> (infoHash, p)) peers) ++ sPeerQueue s }

   NewTorrent infoHash torrent -> do
        debugP "Добавление торрента"
        modify $ \s -> s { sTorrent = M.insert infoHash torrent (sTorrent s) }

   NewIncoming _conn -> do
        undefined

   StopTorrent _ih -> do
        undefined


peerEvent :: PeerEvent -> Process PConf PState ()
peerEvent event = case event of
    Connect _infoHash ->
        newPeer
    Disconnect _infoHash ->
        removePeer
  where
    newPeer = do
        debugP $ "Подключаем пир"
        modify $ \s -> s { sPeerCount = sPeerCount s + 1 }

    removePeer = do
        debugP $ "Отключаем пир"
        modify $ \s -> s { sPeerCount = sPeerCount s - 1 }


fillPeers :: Process PConf PState ()
fillPeers = do
    count <- gets sPeerCount
    when (count < numPeers) $ do
        let toAdd = numPeers - count
        debugP $ "Подключаем новых " ++ show toAdd ++ " пиров"
        queue <- gets sPeerQueue
        let (peers, remain) = splitAt toAdd queue
        modify $ \s -> s { sPeerQueue = remain }
        mapM_ addPeer peers
    return ()


addPeer :: (InfoHash, Peer) -> Process PConf PState ()
addPeer (_infoHash, _peer) = do
    debugP "AddPeer: undefined"
    return ()


