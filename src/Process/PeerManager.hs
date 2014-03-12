module Process.PeerManager
    (
    ) where




data PConf = PConf
    { cPeerManagerChan  :: TChan PeerManagerMessage
    }

data PState = PState
    { sPeerId      :: PeerId                 -- ^ Наш peer_id
    , sPeerCount   :: Integer                -- ^ Кол-во активных пиров
    , sPeerQueue   :: [(InfoHash, Peer)]     -- ^ Очередь пиров для подключения
    , sTorrent     :: M.Map InfoHash Torrent -- ^ Список торрентов. см. Torrent
    }

instance ProcessName PConf where
    processName _ = "PeerManager"


-- ^ Эти данные передаются процессу при подключении пира
data Torrent = Torrent
    { pPieceArray :: PieceArray
    }

data PeerManagerMessage
    = NewTorrent InfoHash TorrentLocal
    | StopTorrent InfoHash
    | PeersFromTracker InfoHash [Peer]
    | NewIncoming (Socket, SockAddr)
    | Connect infoHash peerId
    | Disconnect peerId


numPeers :: Int
numPeers = 40


runPeerManager :: PeerId -> TChan PeerManagerMessage -> IO Reason
runPeerManager peerId statusChan torrentChan torrentManagerChan =
    process0 "PeerManager" conf state wait receive
  where
    conf = PConf peerChan
    state =  PState peerId [] M.empty M.empty


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


fillPeers :: Process PConf PState ()
fillPeers = do
    count <- M.size `fmap` gets sActivePeers
    when (count < numPeers) $ do
        let addCount = numPeers - count
        debugP $ "Подключаем новых " ++ show addCount ++ " пиров"
        queue <- gets sPeerQueue
        let (peers, rest) = splitAt addCount queue
        mapM_ addPeer peers
        modify (\state -> state { sPeerQueue = rest })


addPeer :: (InfoHash, Peer) -> Process PConf PState ThreadId
addPeer (infoHash, Peer addr) = do
    peerId <- gets sPeerId
    -- pool <- asks cPeerPool
    mgrC <- asks mgrCh
    cm   <- gets cChanManageMap
    rateTV <- asks cRateTV
    liftIO $ connect (addr, peerId, infoHash) pool mgrC rateTV cm



incomingPeers :: PeerManagerMessage -> Process PConf PState ()
incomingPeers message = case message of
   PeersFromTracker infoHash peers -> do
        debugP "Добавление новых пиров в очередь"
        modify $ \s -> s { sPeerQueue = (map (infohash,) peers) ++ sPeerQueue s })

   NewTorrent infoHash torrent -> do
        modify $ \s -> s { sChanManageMap = M.insert infoHash torrent (sChanManageMap s) })

   NewIncoming _conn -> do
        undefined

   StopTorrent _ih -> do
        undefined


peerEvent :: PeerMessage -> Process PConf PState ()
peerEvent msg
    = case msg of
        Connect infoHash tid chan ->
            newPeer ih tid chan
        Disconnect tid ->
            removePeer tid
  where
    newPeer infoHash tid chan = do
        debugP $ "Подключаем пир " ++ show tid
        chockChan <- asks chokeMgrCh
        liftIO . atomically $ writeTChan chockChan (AddPeer infoHash tid chan)
        peers <- M.insert tid chan <$> gets sActivePeers
        modify (\s -> s { sActivePeers = peers })

    removePeer threadId = do
        debugP $ "Отключаем пир " ++ show threadId
        chockChan <- asks cChokeManagerChan
        liftIO . atomically $ writeTChan chockChan (RemovePeer threadId)
        peers <- M.delete threadId <$> gets sActivePeers
        modify (\s -> s { sActivePeers = peers })


