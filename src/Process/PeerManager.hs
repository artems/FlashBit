module Process.PeerManager
    ( runPeerManager
    , PeerManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)
import qualified Network.Socket as S

import qualified Data.Map as M

import Torrent
import Process
import Process.Channel
import Process.Status
import Process.FileAgent
import Process.PieceManager as PieceManager
import Process.ChokeManager as ChokeManager


data PeerManagerMessage
    = NewPeers InfoHash [Peer]
    | NewConnection (S.Socket, S.SockAddr)
    | PeerMAddTorrent InfoHash (TVar [UpDownStat]) PieceArray (TChan PieceManagerMessage) (TChan FileAgentMessage)
    | PeerMRemoveTorrent InfoHash


data PeerMTorrent = PeerMTorrent
    { _statV      :: TVar [UpDownStat]
    , _pieceArray :: PieceArray
    , _pieceMChan :: TChan PieceManagerMessage
    , _fsChan     :: TChan FileAgentMessage
    }


data PConf = PConf
    { _rateV      :: RateTVar
    , _peerEChan  :: TChan PeerEventMessage
    , _peerMChan  :: TChan PeerManagerMessage
    , _chokeMChan :: TChan ChokeManagerMessage
    }


instance ProcessName PConf where
    processName _ = "PeerManager"


data PState = PState
    { _peerId     :: PeerId
    , _peerMap    :: M.Map ThreadId (TChan PeerHandlerMessage)
    , _peerQueue  :: [(InfoHash, Peer)]
    , _torrentMap :: M.Map InfoHash PeerMTorrent
    }


maxPeers :: Int
maxPeers = 10

runPeerManager :: PeerId -> RateTVar
    -> TChan PeerManagerMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPeerManager peerId rateV peerMChan chokeMChan = do
    peerEChan <- newTChanIO
    let pconf  = PConf rateV peerEChan peerMChan chokeMChan
        pstate = PState peerId M.empty [] M.empty
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    fillPeers
    process


wait :: Process PConf PState (Either PeerManagerMessage PeerEventMessage)
wait = do
    peerMChan <- asks _peerMChan
    peerEChan <- asks _peerEChan
    liftIO . atomically $
        (readTChan peerMChan >>= return . Left) `orElse`
        (readTChan peerEChan >>= return . Right)


receive :: Either PeerManagerMessage PeerEventMessage -> Process PConf PState ()
receive message = do
    case message of
        Left message  -> receive' message
        Right message -> peerEvent message


receive' :: PeerManagerMessage -> Process PConf PState ()
receive' message =
    case message of
        NewPeers infohash peers -> do
            debugP "Добавляем новых пиров в очередь"
            let peers' = map (\peer -> (infohash, peer)) peers
            modify $ \st -> st { _peerQueue = peers' ++ _peerQueue st }

        NewConnection conn@(socket, _sockaddr) -> do
            peerMap <- gets _peerMap
            if M.size peerMap < maxPeers
                then do
                    debugP "Подключился новый пир"
                    _ <- addConnection conn
                    return ()
                else do
                    debugP "Слишком много пиров, закрываем соединение"
                    liftIO $ S.sClose socket

        PeerMAddTorrent infohash rateV pieceArray pieceMChan fsChan -> do
            let torrent = PeerMTorrent rateV pieceArray pieceMChan fsChan
            modify $ \st -> st { _torrentMap = M.insert infohash torrent (_torrentMap st) }

        PeerMRemoveTorrent _infohash -> do
            errorP $ "Удаление торрента не реализованно"
            stopProcess


peerEvent :: PeerEventMessage -> Process PConf PState ()
peerEvent message =
    case message of
        Connect infohash threadId peerChan -> do
            debugP $ "Добавляем пир " ++ show threadId
            peerMap    <- gets _peerMap
            chokeMChan <- asks _chokeMChan
            liftIO . atomically $ writeTChan chokeMChan $
                ChokeMAddPeer infohash threadId peerChan
            modify $ \st -> st { _peerMap = M.insert threadId peerChan peerMap }

        Disconnect threadId -> do
            debugP $ "Удаляем пир " ++ show threadId
            peerMap    <- gets _peerMap
            chokeMChan <- asks _chokeMChan
            liftIO . atomically $ writeTChan chokeMChan $
                ChokeMRemovePeer threadId
            modify $ \st -> st { _peerMap = M.delete threadId peerMap }


fillPeers :: Process PConf PState ()
fillPeers = do
    peerMap <- gets _peerMap
    let count = M.size peerMap
    when (count < maxPeers) $ do
        queue <- gets _peerQueue
        let (peers, rest) = splitAt (maxPeers - count) queue
        debugP $ "Подключаем дополнительно " ++ show (maxPeers - count) ++ " пиров"
        modify $ \st -> st { _peerQueue = rest }
        mapM_ addPeer peers


addPeer :: (InfoHash, Peer) -> Process PConf PState ()
addPeer (infohash, (Peer addr)) = do
    return ()


addConnection :: (S.Socket, S.SockAddr) -> Process PConf PState ()
addConnection conn = do
    return ()



