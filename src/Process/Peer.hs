{-# LANGUAGE ScopedTypeVariables #-}

module Process.Peer
    ( runPeer
    ) where


import Control.Concurrent.STM
import Control.Exception
import qualified Network.Socket as S (Socket)

import Torrent
import Torrent.Message (Handshake(..))

import Process
import ProcessGroup
import Process.Channel
import Process.Status (UpDownStat)
import Process.FileAgent (FileAgentMessage)
import Process.PieceManager (PieceManagerMessage)
import Process.Peer.Sender (runPeerSender)
import Process.Peer.Handler (runPeerHandler)
import Process.Peer.Receiver (runPeerReceiver)


runPeer :: S.Socket -> PeerId -> [Capabilities] -> RateTVar -> TVar [UpDownStat]
        -> TChan PeerEventMessage-> TChan PieceManagerMessage
        -> TChan FileAgentMessage -> PieceArray -> Int -> InfoHash
        -> IO ()
runPeer socket peerId caps rateV statV peerEventChan pieceMChan fileAgentChan pieceArray nPieces infoHash = do
    sendChan <- newTChanIO
    fromChan <- newTChanIO

    let handshake = Handshake peerId infoHash []
    let allForOne =
            [ runPeerSender socket handshake sendChan
            , runPeerReceiver socket fromChan
            , runPeerHandler fromChan sendChan
            ]

    group  <- initGroup
    result <- runGroup group allForOne
    case result of
        Left (e :: SomeException) -> throwIO e
        _                         -> ioError (userError "Unexpected termination")

{-
-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

data PConf = PConf
    { inCh :: TChan MsgTy
    -- , outCh :: TChan SenderQ.SenderQMsg
    , _peerEventChan :: TChan PeerEventMessage
    , _pieceMChan :: TChan PieceManagerMessage
    , _statV :: TVar [UpDownStat]
    , _rateV :: RateTVar
    , _infoHash :: InfoHash
    , _pieceArray :: PieceArray
    , _piecesDoneV :: TMVar [PieceNum]
    , _haveV :: TMVar [PieceNum]
    , _blockV :: TMVar Blocks
    }


data PState = PState
    { _weChoke        :: Bool -- ^ True if we are choking the peer
    , _weInterested   :: Bool -- ^ True if we are interested in the peer
    , _blockQueue     :: (S.Set (PieceNum, PieceBlock)) -- ^ Blocks queued at the peer
    , _peerChoke      :: Bool -- ^ Is the peer choking us? True if yes
    , _peerInterested :: Bool -- ^ True if the peer is interested
    , _peerPieces     :: (PS.PieceSet) -- ^ List of pieces the peer has access to
    , _missingPieces  :: Int -- ^ Tracks the number of pieces the peer misses before seeding
    , _upRate         :: Rate -- ^ Upload rate towards the peer (estimated)
    , _downRate       :: Rate -- ^ Download rate from the peer (estimated)
    , _runningEndgame :: Bool -- ^ True if we are in endgame
    , _lastMsg        :: Int  -- ^ Ticks from last Message
    , _lastPieceMsg   :: Int  -- ^ Ticks from last Piece Message
    , _interestingPieces :: (S.Set PieceNum) -- ^ peer pieces we are interested in
    }


peerP :: [Capabilities] -> MgrChannel -> ChokeMgr.RateTVar -> PieceMgrChannel -> PieceMap -> Int
         -> TChan SenderQ.SenderQMsg -> TChan MsgTy -> TVar [PStat] -> InfoHash
         -> SupervisorChannel -> IO ThreadId
peerP caps pMgrC rtv pieceMgrC pm nPieces outBound inBound stv ih supC = do
    ct <- getCurrentTime
    pdtmv <- newEmptyTMVarIO
    havetv <- newEmptyTMVarIO
    gbtmv <- newEmptyTMVarIO
    pieceSet <- PS.new nPieces
    let cs = configCapabilities caps
    spawnP (CF inBound outBound pMgrC pieceMgrC stv rtv ih pm
                    pdtmv havetv gbtmv cs)
           (ST True False S.empty True False pieceSet nPieces
                    (RC.new ct) (RC.new ct) False 0 0 S.empty 0)
                       (cleanupP (startup nPieces) (defaultStopHandler supC) cleanup)


startup :: Int -> Process CF ST ()
startup nPieces = do
    tid <- liftIO $ myThreadId
    pmc <- asks peerMgrCh
    ih <- asks pcInfoHash
    ich <- asks inCh
    liftIO . atomically $ writeTChan pmc $ Connect ih tid ich
    pieces <- getPiecesDone
    outChan $ SenderQ.SenderQM $ BitField (constructBitField nPieces pieces)
    -- eventually handle extended messaging
    asks extConf >>= fromLJ sendExtendedMsg
    -- Install the StatusP timer
    _ <- registerSTM 5 ich TimerTick
    eventLoop

cleanup :: Process CF ST ()
cleanup = do
    t <- liftIO myThreadId
    pieces <- gets peerPieces >>= PS.toList
    ch2 <- asks peerMgrCh
    msgPieceMgr (PeerUnhave pieces)
    liftIO . atomically $ writeTChan ch2 (Disconnect t)

readInCh :: Process CF ST MsgTy
readInCh = do
    inb <- asks inCh
    liftIO . atomically $ readTChan inb

eventLoop :: Process CF ST ()
eventLoop = do
    ty <- readInCh
    case ty of
        FromPeer (msg, sz) -> peerMsg msg sz
        TimerTick       ->  timerTick
        FromSenderQ l   ->
                           (do s <- get
                               let !u = RC.update l $ upRate s
                               put $! s { upRate = u})
        FromChokeMgr m  ->  chokeMgrMsg m
    eventLoop

-- | Return a list of pieces which are currently done by us
getPiecesDone :: Process CF ST [PieceNum]
getPiecesDone = do
    c  <- asks piecesDoneTV
    msgPieceMgr (GetDone c)
    liftIO $ do atomically $ takeTMVar c


trackInterestRemove :: PieceNum -> Process CF ST ()
trackInterestRemove pn = do
    im <- gets interestingPieces
    let !ns = S.delete pn im
    if S.null ns
        then do modify (\db -> db { interestingPieces = S.empty,
                                    weInterested = False })
                debugP "We are not interested"
                outChan $ SenderQ.SenderQM NotInterested
        else modify (\db -> db { interestingPieces = ns })


trackInterestAdd :: [PieceNum] -> Process CF ST ()
trackInterestAdd pns = do
    c <- asks haveTV
    msgPieceMgr (PeerHave pns c)
    interesting <- liftIO . atomically $ takeTMVar c
    set <- gets interestingPieces
    let !ns = upd interesting set
    if S.null ns
        then modify (\db -> db { interestingPieces = S.empty })
        else do modify (\db -> db { interestingPieces = ns,
                                    weInterested = True })
                debugP "We are interested"
                outChan $ SenderQ.SenderQM Interested
  where upd is set = foldl (flip S.insert) set is


-- | Process an event from the Choke Manager
chokeMgrMsg :: PeerChokeMsg -> Process CF ST ()
chokeMgrMsg msg = do
   case msg of
       PieceCompleted pn -> do
            debugP "Telling about Piece Completion"
            outChan $ SenderQ.SenderQM $ Have pn
            trackInterestRemove pn
       ChokePeer -> do choking <- gets weChoke
                       when (not choking)
                            (do t <- liftIO myThreadId
                                debugP $ "Pid " ++ show t ++ " choking"
                                outChan $ SenderQ.SenderOChoke
                                modify (\s -> s {weChoke = True}))
       UnchokePeer -> do choking <- gets weChoke
                         when choking
                              (do t <- liftIO myThreadId
                                  debugP $ "Pid " ++ show t ++ " unchoking"
                                  outChan $ SenderQ.SenderQM Unchoke
                                  modify (\s -> s {weChoke = False}))
       CancelBlock pn blk -> do
            cf <- asks extConf
            fromLJ handleCancelMsg cf pn blk


cancelBlock :: PieceNum -> Block -> Process CF ST ()
cancelBlock pn blk = do
    s <- get
    put $! s { blockQueue = S.delete (pn, blk) $ blockQueue s }
    outChan $ SenderQ.SenderQRequestPrune pn blk


checkKeepAlive :: Process CF ST ()
checkKeepAlive = do
    lm <- gets lastMsg
    if lm >= 24
        then do outChan $ SenderQ.SenderQM KeepAlive
        else let !inc = succ lm
             in modify (\st -> st { lastMsg = inc })

isSnubbed :: Process CF ST Bool
isSnubbed = do
    -- 6 * 5 seconds is 30
    lpm <- gets lastPieceMsg
    if lpm > 6
        then return True
        else do
            let !inc = succ lpm
            modify (\st -> st { lastPieceMsg = inc })
            return False

-- A Timer event handles a number of different status updates. One towards the
-- Choke Manager so it has a information about whom to choke and unchoke - and
-- one towards the status process to keep track of uploaded and downloaded
-- stuff.
timerTick :: Process CF ST ()
timerTick = do
   checkKeepAlive
   inC <- asks inCh
   _ <- registerSTM 5 inC TimerTick
   (nur, ndr) <- timerTickChokeMgr
   timerTickStatus nur ndr

-- Tell the ChokeMgr about our progress
timerTickChokeMgr :: Process CF ST (Rate, Rate)
timerTickChokeMgr =  do
   mTid <- liftIO myThreadId
   ur <- gets upRate
   dr <- gets downRate
   t <- liftIO $ getCurrentTime
   let (up, nur) = RC.extractRate t ur
       (down, ndr) = RC.extractRate t dr
   infoP $ "Peer has rates up/down: " ++ show up ++ "/" ++ show down
   i <- gets peerInterested
   seed <- isASeeder
   snub <- isSnubbed
   pchoke <- gets peerChoke
   rtv <- asks rateTV
   let peerInfo = (mTid, ChokeMgr.PRI {
                   ChokeMgr.peerUpRate = up,
                   ChokeMgr.peerDownRate = down,
                   ChokeMgr.peerInterested = i,
                   ChokeMgr.peerSeeding = seed,
                   ChokeMgr.peerSnubs = snub,
                   ChokeMgr.peerChokingUs = pchoke })
   liftIO . atomically $ do
       q <- readTVar rtv
       writeTVar rtv (peerInfo : q)
   return (nur, ndr)


-- Tell the Status Process about our progress
timerTickStatus :: RC.Rate -> RC.Rate -> Process CF ST ()
timerTickStatus nur ndr =  do
   let (upCnt, nuRate) = RC.extractCount $ nur
       (downCnt, ndRate) = RC.extractCount $ ndr
   stv <- asks statTV
   ih <- asks pcInfoHash
   liftIO .atomically $ do
       q <- readTVar stv
       writeTVar stv (PStat { pInfoHash = ih
                            , pUploaded = fromIntegral upCnt
                            , pDownloaded = fromIntegral downCnt } : q)
   modify (\s -> s { upRate = nuRate, downRate = ndRate })


chokeMsg :: Process CF ST ()
chokeMsg = do
    putbackBlocks
    s <- get
    put $! s { peerChoke = True }

unchokeMsg :: Process CF ST ()
unchokeMsg = do
    s <- get
    put $! s { peerChoke = False }
    fillBlocks

-- | Process an Message from the peer in the other end of the socket.
peerMsg :: Message -> Int -> Process CF ST ()
peerMsg msg sz = do
   modify (\s -> s { downRate = RC.update sz $ downRate s})
   case msg of
     KeepAlive  ->  return ()
     Choke      -> asks extConf >>= fromLJ handleChokeMsg
     Unchoke    ->  unchokeMsg
     Interested ->  modify (\s -> s { peerInterested = True })
     NotInterested ->  modify (\s -> s { peerInterested = False })
     Have pn ->  haveMsg pn
     BitField bf -> bitfieldMsg bf
     Request pn blk ->  do
                          cf <- asks extConf
                          fromLJ handleRequestMsg cf pn blk
     Piece n os bs ->  do
                         cf <- asks extConf
                         fromLJ handlePieceMsg cf n os bs
                         modify (\st -> st { lastPieceMsg = 0 })
                         fillBlocks
     Cancel pn blk ->  cancelMsg pn blk
     Port _ -> return () -- No DHT yet, silently ignore
     HaveAll -> fromLJ handleHaveAll =<< asks extConf
     HaveNone -> fromLJ handleHaveNone =<< asks extConf
     Suggest pn -> do cf <- asks extConf
                      fromLJ handleSuggest cf pn
     AllowedFast pn -> do cf <- asks extConf
                          fromLJ handleAllowedFast cf pn
     RejectRequest pn blk -> do cf <- asks extConf
                                fromLJ handleRejectRequest cf pn blk
     ExtendedMsg idx bs -> do cf <- asks extConf
                              fromLJ handleExtendedMsg cf idx bs

-- | Put back blocks for other peer processes to grab. This is done whenever
-- the peer chokes us, or if we die by an unknown cause.
putbackBlocks :: Process CF ST ()
putbackBlocks = do
    blks <- gets blockQueue
    msgPieceMgr (PutbackBlocks (S.toList blks))
    modify (\s -> s { blockQueue = S.empty })


-- | Process a HAVE message from the peer. Note we also update interest as a side effect
haveMsg :: PieceNum -> Process CF ST ()
haveMsg pn = do
    pm <- asks pieceMap
    let (lo, hi) = bounds pm
    if pn >= lo && pn <= hi
        then do PS.insert pn =<< gets peerPieces
                debugP $ "Peer has pn: " ++ show pn
                trackInterestAdd [pn]
                decMissingCounter 1
                fillBlocks
        else do warningP "Unknown Piece"
                stopP

-- True if the peer is a seeder
isASeeder :: Process CF ST Bool
isASeeder = do sdr <- gets missingPieces
               return $! sdr == 0

-- Decrease the counter of missing pieces for the peer
decMissingCounter :: Int -> Process CF ST ()
decMissingCounter n = do
    modify (\s -> s { missingPieces = missingPieces s - n})
    m <- gets missingPieces
    when (m == 0) assertSeeder

-- Assert that the peer is a seeder
assertSeeder :: Process CF ST ()
assertSeeder = do
    ok <- liftM2 (==) (gets peerPieces >>= PS.size) (succ . snd . bounds <$> asks pieceMap)
    assert ok (return ())

-- | Process a BITFIELD message from the peer. Side effect: Consider Interest.
bitfieldMsg :: BitField -> Process CF ST ()
bitfieldMsg bf = do
    pieces <- gets peerPieces
    piecesNull <- PS.null pieces
    if piecesNull
        -- TODO: Don't trust the bitfield
        then do nPieces <- succ . snd . bounds <$> asks pieceMap
                pp <- createPeerPieces nPieces bf
                modify (\s -> s { peerPieces = pp })
                peerLs <- PS.toList pp
                trackInterestAdd peerLs
                decMissingCounter (length peerLs)
        else do infoP "Got out of band Bitfield request, dying"
                stopP

-- | Process a request message from the Peer
requestMsg :: PieceNum -> Block -> Process CF ST ()
requestMsg pn blk = do
    choking <- gets weChoke
    unless choking
        (do debugP $ "Peer requested: " ++ show pn ++ "(" ++ show blk ++ ")"
            outChan $ SenderQ.SenderQPiece pn blk)

requestFastMsg :: PieceNum -> Block -> Process CF ST ()
requestFastMsg pn blk = do
    choking <- gets weChoke
    debugP $ "Peer fastRequested: " ++ show pn ++ "(" ++ show blk ++ ")"
    if choking
        then outChan $ SenderQ.SenderQM (RejectRequest pn blk)
        else outChan $ SenderQ.SenderQPiece pn blk

-- | Handle a Piece Message incoming from the peer
pieceMsg :: PieceNum -> Int -> B.ByteString -> Process CF ST ()
pieceMsg pn offs bs = pieceMsg' pn offs bs >> return ()

fastPieceMsg :: PieceNum -> Int -> B.ByteString -> Process CF ST ()
fastPieceMsg pn offs bs = do
    r <- pieceMsg' pn offs bs
    unless r
        (do infoP "Peer sent out-of-band piece we did not request, closing"
            stopP)

pieceMsg' :: PieceNum -> Int -> B.ByteString -> Process CF ST Bool
pieceMsg' n os bs = do
    let sz = B.length bs
        blk = Block os sz
        e = (n, blk)
    q <- gets blockQueue
    -- When e is not a member, the piece may be stray, so ignore it.
    -- Perhaps print something here.
    if S.member e q
        then do storeBlock n blk bs
                bq <- gets blockQueue >>= return . S.delete e
                s <- get
                bq `deepseq` put $! s { blockQueue = bq }
                return True
        else return False

-- | Handle a cancel message from the peer
cancelMsg :: PieceNum -> Block -> Process CF ST ()
cancelMsg n blk = outChan $ SenderQ.SenderQCancel n blk

-- | Try to fill up the block queue at the peer. The reason we pipeline a
-- number of blocks is to get around the line delay present on the internet.
fillBlocks :: Process CF ST ()
fillBlocks = do
    choked <- gets peerChoke
    interested <- gets weInterested
    when (not choked && interested) checkWatermark

-- | check the current Watermark level. If we are below the lower one, then
-- fill till the upper one. This in turn keeps the pipeline of pieces full as
-- long as the peer is interested in talking to us.
-- TODO: Decide on a queue size based on the current download rate.
checkWatermark :: Process CF ST ()
checkWatermark = do
    q <- gets blockQueue
    eg <- gets runningEndgame
    let sz = S.size q
        mark = if eg then endgameLoMark else loMark
    when (sz < mark)
        (do toQueue <- grabBlocks (hiMark - sz)
            queuePieces toQueue)

-- These three values are chosen rather arbitrarily at the moment.
loMark :: Int
loMark = 5

hiMark :: Int
hiMark = 25

-- Low mark when running in endgame mode
endgameLoMark :: Int
endgameLoMark = 1


-- | Queue up pieces for retrieval at the Peer
queuePieces :: [(PieceNum, Block)] -> Process CF ST ()
queuePieces toQueue = do
    s <- get
    let bq = blockQueue s
    unless (Prelude.null toQueue) $ updateLastPnCache (head toQueue)
    q <- forM toQueue
            (\(p, b) -> do
                if S.member (p, b) bq
                    then return Nothing -- Ignore pieces which are already in queue
                    else do outChan $ SenderQ.SenderQM $ Request p b
                            return $ Just (p, b))
    put $! s { blockQueue = S.union bq (S.fromList $ catMaybes q) }
  where
    updateLastPnCache (pn, _) =
        modify (\s -> s { lastPn = pn })

-- | Tell the PieceManager to store the given block
storeBlock :: PieceNum -> Block -> B.ByteString -> Process CF ST ()
storeBlock n blk bs = msgPieceMgr (StoreBlock n blk bs)

-- | The call @grabBlocks n@ will attempt to grab (up to) @n@ blocks from the
-- piece Manager for request at the peer.
grabBlocks :: Int -> Process CF ST [(PieceNum, Block)]
grabBlocks n = do
    c <- asks grabBlockTV
    ps <- gets peerPieces
    lpn <- gets lastPn
    msgPieceMgr (GrabBlocks n ps c lpn)
    blks <- liftIO $ do atomically $ takeTMVar c
    case blks of
        Leech bs -> return bs
        Endgame bs ->
            modify (\s -> s { runningEndgame = True }) >> return bs


createAllPieces :: MonadIO m => Int -> Bool -> m PS.PieceSet
createAllPieces n False = PS.fromList n []
createAllPieces n True  = PS.fromList n [0..(n-1)]

createPeerPieces :: MonadIO m => Int -> B.ByteString -> m PS.PieceSet
createPeerPieces nPieces =
    PS.fromList nPieces . map fromIntegral . concat . decodeBytes 0 . B.unpack
  where decodeByte :: Int -> Word8 -> [Maybe Int]
        decodeByte soFar w =
            let dBit n = if testBit w (7-n)
                           then Just (n+soFar)
                           else Nothing
            in fmap dBit [0..7]
        decodeBytes _ [] = []
        decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws

-- | Send a message on a chan from the process queue
outChan :: SenderQ.SenderQMsg -> Process CF ST ()
outChan qm = do
    modify (\st -> st { lastMsg = 0 })
    ch <- asks outCh
    liftIO . atomically $ writeTChan ch qm

msgPieceMgr :: PieceMgrMsg -> Process CF ST ()
msgPieceMgr m = do
   pmc <- asks pieceMgrCh
   liftIO . atomically $ writeTChan pmc m

-}
