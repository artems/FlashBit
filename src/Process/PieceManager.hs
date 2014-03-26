module Process.PieceManager
    ( runPieceManager
    , PieceManagerMessage(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)

import Torrent
import Process
import Process.Status
import Process.FileAgent
import Process.ChokeManager


data PieceManagerMessage = PMM

data PConf = PConf
    { _infoHash   :: InfoHash
    , _fsChan     :: TChan FileAgentMessage
    , _statusChan :: TChan StatusMessage
    , _pieceMChan :: TChan PieceManagerMessage
    , _chokeMChan :: TChan ChokeManagerMessage
    }


instance ProcessName PConf where
    processName _ = "PieceManager"


data PState = PState
    { _donePush      :: [ChokeManagerMessage] -- ^ Pieces that should be pushed to the Choke Mgr.
    , _pieces        :: M.Map PieceNum PieceState -- ^ Pieces in progress
    , _downloading   :: S.Set (PieceNum, PieceBlock) -- ^ Blocks we are currently downloading
    , _infoMap       :: PieceArray   -- ^ Information about pieces
    , _endGaming     :: Bool       -- ^ If we have done any endgame work this is true
    , _histogram     :: PH.PieceHistogram -- ^ Track the rarity of pieces
    , _assertCount   :: Int        -- ^ When to next check the database for consistency
    }



runPieceManager :: InfoHash -> PieceArray -> PieceHaveMap
    -> TChan PieceManagerMessage
    -> TChan FileAgentMessage
    -> TChan StatusMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPieceManager infohash pieceArray pieceHaveMap pieceMChan fsChan statusChan chokeChan = do
    let pconf  = PConf ()
        pstate = PState ()
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    liftIO $ threadDelay $ 1000 * 1000
    process



data PieceState
    = PieceDone
    | PiecePending
    | PieceInProgress
        { _pieceDone         :: Int -- ^ Number of blocks when piece is done
        , _pieceHaveBlock    :: S.Set Block -- ^ The blocks we have
        , _piecePendingBlock :: [PieceBlock] -- ^ Blocks still pending
        } deriving (Show)


sizeReport :: Process PConf PState String
sizeReport = do
    (PState _ _ down _ _ histo _ _) <- get
    return $ show
         [ ("Downloading", S.size down)
         , ("Histo", PH.size histo)
         ]


data Blocks = Leech [(PieceNum, Block)]
            | Endgame [(PieceNum, Block)]

data PieceManagerMessage
    = GrabBlocks Int PS.PieceSet (TMVar Blocks) PieceNum
      -- ^ Ask for grabbing some blocks
    | StoreBlock PieceNum PieceBlock B.ByteString
      -- ^ Ask for storing a block on the file system
    | PutbackBlocks [(PieceNum, PieceBlock)]
      -- ^ Put these blocks back for retrieval
    | GetDone (TMVar [PieceNum])
      -- ^ Get the pieces which are already done
    | PeerHave [PieceNum] (TMVar [PieceNum])
      -- ^ A peer has the given piece(s)
    | PeerUnhave [PieceNum]
      -- ^ A peer relinquished the given piece Indexes


instance Show PieceMgrMsg where
    show (GrabBlocks x _ _ _)  = "GrabBlocks " ++ show x
    show (StoreBlock pn blk _) = "StoreBlock " ++ show pn ++ " " ++ show blk
    show (PutbackBlocks x)     = "PutbackBlocks " ++ show x
    show (GetDone _)           = "GetDone"
    show (PeerHave xs _)       = "PeerHave " ++ show xs
    show (PeerUnhave xs)       = "PeerUnhave " ++ show xs



start :: PieceMgrChannel -> FSPChannel -> ChokeMgrChannel -> StatusChannel -> ST -> InfoHash
      -> SupervisorChannel -> IO ThreadId
start mgrC fspC chokeC statC db ih supC =
    spawnP (CF mgrC fspC chokeC statC ih) db
                    ({-# SCC "PieceMgr" #-} catchP eventLoop
                        (defaultStopHandler supC))

eventLoop :: Process CF ST ()
eventLoop = do
    assertST
    rpcMessage
    drainSend
    eventLoop

drainSend :: Process CF ST ()
drainSend = {-# SCC "drainSend" #-} do
    dl <- gets donePush
    if (null dl)
        then return ()
        else do
          c <- asks chokeCh
          liftIO . atomically $ writeTChan c (head dl)
          s <- get
          put $! s { donePush = tail (donePush s) }

traceMsg :: PieceMgrMsg -> Process CF ST ()
traceMsg m = {-# SCC "traceMsg" #-} do
    tb <- gets traceBuffer
    let !ntb = (trace $! show m) tb
    db <- get
    put $! db { traceBuffer = ntb }

rpcMessage :: Process CF ST ()
rpcMessage = do
    ch <- asks pieceMgrCh
    m <- {-# SCC "Channel_Read" #-} liftIO . atomically $ readTChan ch
    traceMsg m
    case m of
      GrabBlocks n eligible c lastpn -> {-# SCC "GrabBlocks" #-}
          do blocks <- grabBlocks n eligible lastpn
             liftIO . atomically $ do putTMVar c blocks -- Is never supposed to block
      StoreBlock pn blk d ->
          storeBlock pn blk d
      PutbackBlocks blks -> {-# SCC "PutbackBlocks" #-}
          mapM_ putbackBlock blks
      GetDone c -> {-# SCC "GetDone" #-} do
         done <- doneKeys <$> gets pieces
         liftIO . atomically $ do putTMVar c done -- Is never supposed to block either
      PeerHave idxs c -> peerHave idxs c
      PeerUnhave idxs -> peerUnhave idxs

doneKeys :: M.Map PieceNum PieceSt -> [PieceNum]
doneKeys = M.keys . M.filter f
  where f Done = True
        f _    = False

storeBlock :: PieceNum -> Block -> B.ByteString -> Process CF ST ()
storeBlock pn blk d = {-# SCC "storeBlock" #-} do
     writeFS
     dld <- gets downloading
     let !ndl = S.delete (pn, blk) dld
     s <- get
     put $! s { downloading = ndl }
     endgameBroadcast pn blk
     done <- updateProgress pn blk
     when done (pieceDone pn)
  where writeFS = {-# SCC "writeFS" #-} do
                fch <- asks fspCh
                liftIO . atomically $ writeTChan fch $ WriteBlock pn blk d

pieceDone :: PieceNum -> Process CF ST ()
pieceDone pn = {-# SCC "pieceDone" #-} do
    assertPieceComplete pn
    debugP $ "Marking piece #" ++ show pn ++ " done"
    pieceOk <- checkPiece pn
    case pieceOk of
      Nothing ->
             do fail "PieceMgrP: Piece Nonexisting!"
      Just True -> do completePiece pn
                      markDone pn
                      checkFullCompletion
                      l <- gets infoMap >>= (\pm -> return $! len . (pm !) $ pn)
                      ih <- asks pMgrInfoHash
                      c <- asks statusCh
                      liftIO . atomically $ writeTChan c (CompletedPiece ih l)
      Just False -> putbackPiece pn

peerHave :: [PieceNum] -> TMVar [PieceNum] -> Process CF ST ()
peerHave idxs tmv = {-# SCC "peerHave" #-} do
    ps <- gets pieces
    let !interesting = filter (mem ps) idxs
    liftIO . atomically $ putTMVar tmv interesting
    if null interesting
        then return ()
        else do
            db <- get
            put $! db { histogram = PendS.haves interesting (histogram db)}
  where mem ps p =
                case M.lookup p ps of
                    Nothing -> False
                    Just Done -> False
                    Just _ -> True

peerUnhave :: [PieceNum] -> Process CF ST ()
peerUnhave idxs = {-# SCC "peerUnhave" #-}
    modify (\db -> db { histogram = PendS.unhaves idxs (histogram db)})

endgameBroadcast :: PieceNum -> Block -> Process CF ST ()
endgameBroadcast pn blk = {-# SCC "endgameBroadCast" #-} do
    ih <- asks pMgrInfoHash
    gets endGaming >>=
      flip when
        (do dp <- gets donePush
            let dp' = (BlockComplete ih pn blk) : dp
            dp' `deepseq` modify (\db -> db { donePush = dp' }))

markDone :: PieceNum -> Process CF ST ()
markDone pn = do
    ih <- asks pMgrInfoHash
    modify (\db -> db { donePush = (PieceDone ih pn) : donePush db })

checkPiece :: PieceNum -> Process CF ST (Maybe Bool)
checkPiece n = {-# SCC "checkPiece" #-} do
    v <- liftIO newEmptyTMVarIO
    fch <- asks fspCh
    liftIO $ do
        atomically $ writeTChan fch $ CheckPiece n v
        atomically $ takeTMVar v

createPieceDb :: MonadIO m => PiecesDoneMap -> PieceMap -> m ST
createPieceDb mmap pmap = do
    return $ ST [] (M.map f mmap) S.empty pmap False PendS.empty 0 (Tracer.new 50)
  where f False = Pending
        f True  = Done

-- | The call @completePiece db pn@ will mark that the piece @pn@ is completed
completePiece :: PieceNum -> PieceMgrProcess ()
completePiece pn = do
    modify (\db -> db { pieces = M.update f pn (pieces db),
                        histogram  = PendS.remove pn (histogram db )})
  where f (InProgress _ _ _) = Just Done
        f _                  = error "Impossible (completePiece)"

piecesDone :: M.Map PieceNum PieceSt -> Int
piecesDone m = M.size $ M.filter f m
  where f Done = True
        f _    = False

-- | Handle torrent completion
checkFullCompletion :: PieceMgrProcess ()
checkFullCompletion = do
    ps <- gets pieces
    im <- gets infoMap
    let !donePSz = piecesDone ps
    when (succ (snd (bounds im)) == donePSz)
        (do liftIO $ putStrLn "Torrent Completed; to honor the torrent-gods thou must now sacrifice a goat!"
            ih <- asks pMgrInfoHash
            asks statusCh >>= (\ch -> liftIO . atomically $ writeTChan ch (STP.TorrentCompleted ih))
            c <- asks chokeCh
            liftIO . atomically $ writeTChan c (TorrentComplete ih))

-- | The call @putBackPiece db pn@ will mark the piece @pn@ as not being complete
--   and put it back into the download queue again.
putbackPiece :: PieceNum -> PieceMgrProcess ()
putbackPiece pn = do
    modify (\db -> db { pieces = M.alter f pn (pieces db) })
  where f (Just (InProgress _ _ _)) = Just Pending
        f _                         = error "Impossible (putbackPiece)"

-- | Put back a block for downloading.
putbackBlock :: (PieceNum, Block) -> PieceMgrProcess ()
putbackBlock (pn, blk) = do
    ps <- gets pieces
    case M.lookup pn ps of
        Nothing -> fail "Impossible (Wrong pn)"
        Just Pending -> fail "Impossible, perhaps stray"
        Just Done -> return () -- Stray block at endgame
        Just ipp -> do
            modify (\db -> db { pieces = M.insert pn (ndb ipp) ps,
                                downloading = S.delete (pn, blk) $ downloading db })
  where ndb ipp | S.member blk (ipHaveBlocks ipp) = ipp
                | otherwise = ipp { ipPendingBlocks = blk : ipPendingBlocks ipp}

-- | Assert that a Piece is Complete. Can be omitted when we know it works
--   and we want a faster client.
assertPieceComplete :: PieceNum -> PieceMgrProcess ()
assertPieceComplete pn = {-# SCC "assertPieceComplete" #-} do
    ps <- gets pieces
    ipp <- case M.lookup pn ps of
                Nothing -> fail "assertPieceComplete: Could not lookup piece number"
                Just x -> return x
    dl <- gets downloading
    pm <- gets infoMap
    let sz = len (pm ! pn)
    unless (assertAllDownloaded dl pn)
      (fail "Could not assert that all pieces were downloaded when completing a piece")
    unless (assertComplete ipp sz)
      (fail $ "Could not assert completion of the piece #" ++ show pn
                ++ " with block state " ++ show ipp)
  where assertComplete ip sz = checkContents 0 (fromIntegral sz) (S.toAscList (ipHaveBlocks ip))
        -- Check a single block under assumptions of a cursor at offs
        checkBlock (offs, l, state) blk = (offs + blockSize blk,
                                           l - blockSize blk,
                                           state && offs == blockOffset blk)
        checkContents os l blks = case foldl checkBlock (os, l, True) blks of
                                    (_, 0, True) -> True
                                    _            -> False
        assertAllDownloaded blocks p = all (\(p', _) -> p /= p') $ S.toList blocks

-- | Update the progress on a Piece. When we get a block from the piece, we will
--   track this in the Piece Database. This function returns @complete@
--   where @complete@ is @True@ if the piece is percieved to be complete and @False@
--   otherwise.
updateProgress :: PieceNum -> Block -> PieceMgrProcess Bool
updateProgress pn blk = {-# SCC "updateProgress" #-} do
    ps <- gets pieces
    case M.lookup pn ps of
      Nothing -> fail "Impossible (wrong PN, updateProgress)"
      Just Pending -> fail "Impossible (updateProgress, Pending)"
      Just Done    -> return False -- This happens when a stray block is downloaded
                                   -- TODO: Consider handling it elsewhere in the stack
      Just pg ->
          let blkSet = ipHaveBlocks pg
          in if blk `S.member` blkSet
               then return False -- Stray block download.
                                 -- Will happen without FAST extension
                                 -- at times
               else do
                let pg' = pg { ipHaveBlocks = S.insert blk blkSet }
                db <- get
                put $! db { pieces = M.insert pn pg' (pieces db) }
                return (ipHave pg' == ipDone pg')
  where ipHave = {-# SCC "updateProgress_ipHave" #-} S.size . ipHaveBlocks

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build 0        _os acc = reverse acc
        build leftBytes os acc | leftBytes >= blockSz =
                                     build (leftBytes - blockSz)
                                           (os + blockSz)
                                           $ Block os blockSz : acc
                                 | otherwise = build 0 (os + leftBytes) $ Block os leftBytes : acc

-- | The call @grabBlocks n eligible@ tries to pick off up to @n@ pieces from
--   to download. In doing so, it will only consider pieces in @eligible@. It
--   returns a list of Blocks which where grabbed.
grabBlocks :: Int -> PS.PieceSet -> PieceNum -> PieceMgrProcess Blocks
grabBlocks k eligible lastpn = {-# SCC "grabBlocks" #-} do
    blocks <- tryGrab k eligible lastpn
    ps <- gets pieces
    let pendN = M.null $ M.filter (\a -> case a of Pending -> True
                                                   _       -> False) ps
    if blocks == [] && pendN
        then do blks <- grabEndGame k eligible
                db <- get
                put $! db { endGaming = True }
                debugP $ "PieceMgr entered endgame."
                return $ Endgame blks
        else do s <- get
                let !dld = downloading s
                put $! s { downloading = foldl' (flip S.insert) dld blocks }
                return $ Leech blocks

inProgressPieces :: M.Map PieceNum PieceSt -> [PieceNum]
inProgressPieces m = M.keys $ M.filter f m
  where f Done    = False
        f Pending = False
        f _       = True

-- Grabbing blocks is a state machine implemented by tail calls
-- Try grabbing pieces from the pieces in progress first
tryGrab :: PieceNum -> PS.PieceSet -> PieceNum -> Process CF ST [(PieceNum, Block)]
tryGrab k ps lastpn = {-# SCC "tryGrabProgress" #-}
    tryGrabProgress k ps [] =<< ipp
  where
    ipp :: Process CF ST [PieceNum]
    ipp = do
            p <- gets pieces
            let inProgress = inProgressPieces p
            case M.lookup lastpn p of
                Just (InProgress _ _ _) -> return $ lastpn : inProgress
                _                       -> return $ inProgress

tryGrabProgress :: PieceNum -> PS.PieceSet -> [(PieceNum, Block)] -> [PieceNum]
                -> Process CF ST [(PieceNum, Block)]
tryGrabProgress 0 _ captured _ = return captured
tryGrabProgress k ps captured [] = {-# SCC "tryGrabProgress_k_e" #-}
        tryGrabPending k ps captured
tryGrabProgress k ps captured (i : is) = {-# SCC "tryGrabProgress_k_is" #-}
    do m <- PS.member i ps
       if m
         then grabFromProgress k ps i captured is
         else tryGrabProgress k ps captured is

-- The Piece @p@ was found, grab it
grabFromProgress :: PieceNum -> PS.PieceSet -> PieceNum -> [(PieceNum, Block)] -> [PieceNum]
                 -> Process CF ST [(PieceNum, Block)]
grabFromProgress n ps p captured nxt = {-# SCC "grabFromProgress" #-} do
    pie <- gets pieces
    ipp <- case M.lookup p pie of
              Nothing -> fail "grabFromProgress: could not lookup piece"
              Just Done -> fail "Impossible (Done, grabFromProgress)"
              Just Pending -> fail "Impossible (Pending, grabFromProgress)"
              Just x -> return x
    let (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
        nIpp = ipp { ipPendingBlocks = rest }
    -- This rather ugly piece of code should be substituted with something better
    if grabbed == []
        then tryGrabProgress n ps captured nxt
        else do modify (\db -> db { pieces = M.insert p nIpp pie })
                tryGrabProgress
                    (n - length grabbed) ps
                    ([(p,g) | g <- grabbed] ++ captured)
                    nxt

isPendingPiece :: PieceNum -> M.Map PieceNum PieceSt -> Bool
isPendingPiece pn m = case M.lookup pn m of
                        Just Pending -> True
                        _            -> False

-- Try grabbing pieces from the pending blocks
tryGrabPending :: PieceNum -> PS.PieceSet -> [(PieceNum, Block)]
               -> Process CF ST [(PieceNum, Block)]
tryGrabPending n ps captured = {-# SCC "tryGrabPending" #-} do
    histo <- gets histogram
    pie <- gets pieces
    culprits <- {-# SCC "PendS.pick" #-}
        liftIO $ PendS.pick (\p -> do let pend = isPendingPiece p pie
                                      mem <- PS.member p ps
                                      return (pend && mem))
                            histo
    case culprits of
        Nothing -> return captured
        Just pies -> do
            h <- pickRandom pies
            blockList <- createBlock h
            let sz  = length blockList
                ipp = InProgress sz S.empty blockList
            modify (\db -> db { pieces = M.insert h ipp pie })
            tryGrabProgress n ps captured [h]

grabEndGame :: PieceNum -> PS.PieceSet -> Process CF ST [(PieceNum, Block)]
grabEndGame n ps = {-# SCC "grabEndGame" #-} do
    -- In endgame we are allowed to grab from the downloaders
    dls <- filterM (\(p, _) -> PS.member p ps) =<< (S.toList <$> gets downloading)
    take n . shuffle' dls (length dls) <$> liftIO newStdGen

-- | Pick a random element among a finite list af them.
pickRandom :: MonadIO m => [a] -> m a
pickRandom ls = do
    n <- liftIO $ getStdRandom (\gen -> randomR (0, length ls - 1) gen)
    return $ ls !! n

-- | If given a Piece number, convert said number into its list of blocks to
-- download at peers.
createBlock :: PieceNum -> PieceMgrProcess [Block]
createBlock pn = do
     gets infoMap >>= (\im -> return $! cBlock $ im ! pn)
         where cBlock = blockPiece defaultBlockSize . fromInteger . len

assertST :: PieceMgrProcess ()
assertST = {-# SCC "assertST" #-} do
    c <- gets assertCount
    if c == 0
        then do modify (\db -> db { assertCount = 50 })
                assertSets >> assertDownloading
                sizes <- sizeReport
                debugP sizes
        else do
            db <- get
            put $! db { assertCount = assertCount db - 1 }
  where
    -- If a piece is pending in the database, we have the following rules:
    --
    --  - It is not done.
    --  - It is not being downloaded
    --  - It is not in progresss.
    --
    -- If a piece is done, we have the following rules:
    --
    --  - It is not in progress.
    --  - There are no more downloading blocks.
    assertSets = do return ()
        {-
        down    <- map fst . S.toList <$> gets downloading
        pdownis <- anyM (flip PS.member pending) down
        donedownis <- anyM (flip PS.member done) down

        when pdownis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())
        when donedownis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())

        -}
    -- If a piece is in Progress, we have:
    --
    --  - There is a relationship with what pieces are downloading
    --    - If a block is ipPending, it is not in the downloading list
    --    - If a block is ipHave, it is not in the downloading list
    assertDownloading = do
        pie <- gets pieces
        tr   <- gets traceBuffer
        mapM_ (checkDownloading pie tr) =<< S.toList <$> gets downloading
    checkDownloading pie tr (pn, blk) = do
        case M.lookup pn pie of
            Nothing -> fail $ "Piece " ++ show pn ++ " not in progress while We think it was"
            Just Pending -> fail "Impossible (checkDownloading, Pending)"
            Just Done    -> fail "Impossible (checkDownloading, Done)"
            Just ipp -> do
                when (blk `elem` ipPendingBlocks ipp)
                    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the Pending Block list")
                when (S.member blk $ ipHaveBlocks ipp)
                    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the HaveBlocks set" ++
                        "Trace: " ++ show tr)


