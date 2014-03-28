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
    , _pieceMChan :: TChan PieceManagerMessage
    , _fsChan     :: TChan FileAgentMessage
    , _statusChan :: TChan StatusMessage
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
runPieceManager infohash pieceArray pieceHaveMap pieceMChan fsChan statusChan chokeMChan = do
    let pconf  = PConf infohash pieceMChan fsChan statusChan chokeMChan
        pstate = createPieceDB piecesHaveMap pieceArray
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    assertST
    message <- wait
    receive message
    drainSend
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


eventLoop :: Process PConf PState ()
eventLoop = do


drainSend :: Process PConf PState ()
drainSend = do
    donePush <- gets _donePush
    if (null donePush)
        then return ()
        else do
          chokeMChan <- asks _chokeMChan
          liftIO . atomically $ writeTChan chokeMChan (head donePush)
          modify $ \st -> st { _donePush = tail (_donePush st) }


wait :: Process PConf PState PieceManagerMessage
wait = do
    pieceMChan <- asks _pieceMChan
    liftIO . atomically $ readTChan pieceMChan


receive :: PieceManagerMessage -> Process PConf PState ()
receive message = do
    case message of
        GrabBlocks n eligible c lastpn -> do
            blocks <- grabBlocks n eligible lastpn
            liftIO . atomically $ putTMVar c blocks -- Is never supposed to block

        StoreBlock pieceNum block d ->
            storeBlock pieceNum block d

        PutbackBlocks blocks ->
            mapM_ putbackBlock blocks

        GetDone c -> do
            done <- doneKeys `fmap` gets pieces
            liftIO . atomically $ do putTMVar c done -- Is never supposed to block either

        PeerHave idxs c -> peerHave idxs c

        PeerUnhave idxs -> peerUnhave idxs


doneKeys :: M.Map PieceNum PieceState -> [PieceNum]
doneKeys = M.keys . M.filter f
  where
    f Done = True
    f _    = False


storeBlock :: PieceNum -> PieceBlock -> B.ByteString -> Process PConf PState ()
storeBlock pieceNum block bs = do
    fsChan <- asks _fsChan
    liftIO . atomically $ writeTChan fsChan $ WriteBlock pieceNum block bs
    downloading <- gets _downloading
    modify $ \st -> st { _downloading =  S.delete (piecenum, block) downloading }
    endgameBroadcast pieceNum block
    done <- updateProgress pieceNum block
    when done $ pieceDone pieceNum


pieceDone :: PieceNum -> Process PConf PState ()
pieceDone pieceNum = do
    assertPieceComplete pieceNum
    debugP $ "Marking piece #" ++ show pn ++ " done"
    pieceOk <- checkPiece pieceNum
    case pieceOk of
        Nothing -> do
            errorP "Piece non-existing!"
            stopProcess

        Just True -> do
            infohash   <- asks _infoHash
            statusChan <- asks _statusChan
            completePiece pieceNum
            markDone pieceNum
            checkFullCompletion
            infoMap <- gets _infoMap
            let len = _pieceLength $ infoMap ! pieceNum
            liftIO . atomically $ writeTChan statusChan $
                CompletedPiece infohash len

        Just False ->
            putbackPiece pieceNum


peerHave :: [PieceNum] -> TMVar [PieceNum] -> Process PConf PState ()
peerHave idxs tmv = do
    pieces <- gets _pieces
    let interesting = filter (mem pieces) idxs
    liftIO . atomically $ putTMVar tmv interesting
    if null interesting
        then return ()
        else do
            modify $ \st -> st { _histogram = PH.allHave interesting (_histogram st) }
  where
    mem ps p = case M.lookup p ps of
        Nothing   -> False
        Just Done -> False
        Just _    -> True


peerUnhave :: [PieceNum] -> Process PConf PState ()
peerUnhave idxs = do
    modify $ \st -> st { _histogram = PH.allUnhave idxs (_histogram st) }


endgameBroadcast :: PieceNum -> PieceBlock -> Process PConf PState ()
endgameBroadcast pieceNum block = do
    infohash  <- asks _infoHash
    endGaming <- gets _endGaming
    when endGaming $ do
        donePush <- gets _donePush
        let donePush' = BlockComplete infohash pieceNum block : donePush
        modify $ \st -> st { _donePush = donePush' }


markDone :: PieceNum -> Process PConf PState ()
markDone pieceNum = do
    infohash <- asks _infoHash
    modify $ \st -> st { _donePush = PieceDone infohash pieceNum : _donePush st }


checkPiece :: PieceNum -> Process PConf PState (Maybe Bool)
checkPiece pieceNum = do
    fsChan <- asks _fsChan
    checkV <- liftIO newEmptyTMVarIO
    liftIO $ do
        atomically $ writeTChan fsChan $ CheckPiece pieceNum checkV
        atomically $ takeTMVar checkV


createPieceDb :: PiecesHaveMap -> PieceArray -> IO PState
createPieceDb pieceHaveMap pieceArray =
    PState [] (M.map f pieceHaveMap) S.empty pieceArray False PH.empty 0
  where
    f True  = Done
    f False = Pending


completePiece :: PieceNum -> Process PConf PState ()
completePiece pieceNum = do
    modify $ \st -> st { _pieces = M.update f pieceNum (_pieces st)
                       , _histogram  = PH.remove pieceNum (_histogram st)
                       }
  where
    f (InProgress _ _ _) = Just Done
    f _                  = error "completePiece: impossible"


piecesDone :: M.Map PieceNum PieceState -> Int
piecesDone pieceMap = M.size $ M.filter f pieceMap
  where
    f Done = True
    f _    = False


checkFullCompletion :: Process PConf PState ()
checkFullCompletion = do
    pieces  <- gets _pieces
    infoMap <- gets _infoMap
    let donePSz = piecesDone pieces
    when (succ (snd (bounds infoMap)) == donePSz) $ do
        infoP "Torrent Completed; to honor the torrent-gods thou must now sacrifice a goat!"
        infohash   <- asks _infoHash
        chokeMChan <- asks _chokeMChan
        statusChan <- asks _statusChan
        liftIO . atomically $ writeTChan statusChan $ TorrentCompleted infohash
        liftIO . atomically $ writeTChan chokeMChan $ ChokeMTorrentComplete infohash


putbackPiece :: PieceNum -> Process PConf PState ()
putbackPiece pieceNum = do
    modify $ \st -> st { _pieces = M.alter f pieceNum (_pieces st) }
  where
    f (Just (InProgress _ _ _)) = Just Pending
    f _                         = error "putbackPiece: impossible"


putbackBlock :: (PieceNum, Block) -> Process PConf PState ()
putbackBlock (pieceNum, block) = do
    pieces <- gets _pieces
    case M.lookup pieceNum pieces of
        Nothing      -> fail "Impossible (Wrong pn)"
        Just Pending -> fail "Impossible, perhaps stray"
        Just Done    -> return () -- Stray block at endgame
        Just piece   -> do
            modify $ \st -> st { _pieces = M.insert pieceNum (ndb piece) pieces
                               , _downloading = S.delete (pieceNum, block) (_downloading st) }
  where
    ndb ipp
        | S.member block (_pieceHaveBlock piece) = piece
        | otherwise = piece { _piecePendingBlock = block : _piecePendingBlock piece }


-- | Assert that a Piece is Complete. Can be omitted when we know it works
--   and we want a faster client.
assertPieceComplete :: PieceNum -> PieceMgrProcess ()
assertPieceComplete pieceNum = {-# SCC "assertPieceComplete" #-} do
    pieces <- gets _pieces
    ppiece <- case M.lookup pieceNum pieces of
        Nothing -> fail "assertPieceComplete: Could not lookup piece number"
        Just p  -> return p

    infoMap     <- gets _infoMap
    downloading <- gets _downloading
    let sz = len (infoMap ! pieceNum)
    unless (assertAllDownloaded downloading pieceNum) $
        fail "Could not assert that all pieces were downloaded when completing a piece"
    unless (assertComplete ppiece sz) $
        fail $ "Could not assert completion of the piece #" ++ show pieceNum
             ++ " with block state " ++ show ppiece

  where
    assertComplete ip sz = checkContents 0 (fromIntegral sz) (S.toAscList (ipHaveBlocks ip))
    -- Check a single block under assumptions of a cursor at offs
    checkBlock (offs, l, state) blk =
        (offs + blockSize blk,
        , l - blockSize blk
        , state && offs == blockOffset blk
        )
    checkContents os l blks = case foldl checkBlock (os, l, True) blks of
        (_, 0, True) -> True
        _            -> False
    assertAllDownloaded blocks p = all (\(p', _) -> p /= p') $ S.toList blocks


updateProgress :: PieceNum -> PieceBlock -> Process PConf PState Bool
updateProgress pieceNum block = do
    pieces <- gets _pieces
    case M.lookup pieceNum pieces of
        Nothing      -> fail "Impossible (wrong PN, updateProgress)"
        Just Pending -> fail "Impossible (updateProgress, Pending)"
        Just Done    -> return False -- This happens when a stray block is downloaded
                                   -- TODO: Consider handling it elsewhere in the stack
        Just piece ->
            let blockSet = _pieceHaveBlock piece
            in  if block `S.member` blockSet
                    then return False -- Stray block download.
                                 -- Will happen without FAST extension
                                 -- at times
                    else do
                        let pg' = pg { _pieceHaveBlock = S.insert block blockSet }
                        db <- get
                        modify $ \st -> st { _pieces = M.insert pieceNum pg' (_pieces st) }
                        return (pieceHave pg' == _pieceDone pg')
  where
    pieceHave = S.size . _pieceHaveBlock


blockPiece :: BlockSize -> PieceSize -> [PieceBlock]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where
    build 0        _os acc = reverse acc
    build leftBytes os acc
        | leftBytes >= blockSz =
            build (leftBytes - blockSz) (os + blockSz) $ Block os blockSz : acc
        | otherwise = build 0 (os + leftBytes) $ Block os leftBytes : acc


grabBlocks :: Int -> PS.PieceSet -> PieceNum -> Process PConf PState Blocks
grabBlocks k eligible lastpn = do
    blocks <- tryGrab k eligible lastpn
    pieces <- gets _pieces
    let pendN = M.null $ M.filter (\a -> case a of Pending -> True
                                                   _       -> False) pieces
    if blocks == [] && pendN
        then do
            blks <- grabEndGame k eligible
            modify $ \st -> st { _endGaming = True }
            return $ Endgame blocks
        else do
            modify $ \st -> st { _downloading = foldl' (flip S.insert) (_downloading st) blocks }
            return $ Leech blocks


inProgressPieces :: M.Map PieceNum PieceState -> [PieceNum]
inProgressPieces m = M.keys $ M.filter f m
  where
    f Done    = False
    f Pending = False
    f _       = True


tryGrab :: PieceNum -> PS.PieceSet -> PieceNum -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrab k ps lastpn =
    tryGrabProgress k ps [] =<< ipp
  where
    ipp :: Process PConf PState [PieceNum]
    ipp = do
        p <- gets pieces
        let inProgress = inProgressPieces p
        case M.lookup lastpn p of
            Just (InProgress _ _ _) -> return $ lastpn : inProgress
            _                       -> return $ inProgress


tryGrabProgress :: PieceNum -> PS.PieceSet -> [(PieceNum, PieceBlock)] -> [PieceNum]
                -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrabProgress 0 _  captured _  = return captured
tryGrabProgress k ps captured [] = tryGrabPending k ps captured
tryGrabProgress k ps captured (i : is) =
    do m <- PS.member i ps
        if m
            then grabFromProgress k ps i captured is
            else tryGrabProgress k ps captured is


grabFromProgress :: PieceNum -> PS.PieceSet -> PieceNum -> [(PieceNum, PieceBlock)] -> [PieceNum]
                 -> Process PConf PState [(PieceNum, PieceBlock)]
grabFromProgress n ps p captured nxt = do
    pie <- gets _pieces
    ipp <- case M.lookup p pie of
              Nothing      -> fail "grabFromProgress: could not lookup piece"
              Just Done    -> fail "Impossible (Done, grabFromProgress)"
              Just Pending -> fail "Impossible (Pending, grabFromProgress)"
              Just x       -> return x
    let (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
        nIpp = ipp { ipPendingBlocks = rest }
    -- This rather ugly piece of code should be substituted with something better
    if grabbed == []
        then tryGrabProgress n ps captured nxt
        else do
            modify $ \st -> st { _pieces = M.insert p nIpp pie }
            tryGrabProgress
                    (n - length grabbed) ps
                    ([(p,g) | g <- grabbed] ++ captured)
                    nxt


isPendingPiece :: PieceNum -> M.Map PieceNum PieceState -> Bool
isPendingPiece pn m =
    case M.lookup pn m of
        Just Pending -> True
        _            -> False


-- Try grabbing pieces from the pending blocks
tryGrabPending :: PieceNum -> PS.PieceSet -> [(PieceNum, Block)]
               -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrabPending n ps captured = do
    histo <- gets _histogram
    pie <- gets pieces
    culprits <-
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


grabEndGame :: PieceNum -> PS.PieceSet -> Process PConf PState [(PieceNum, PieceBlock)]
grabEndGame pieceNum pieces = do
    -- In endgame we are allowed to grab from the downloaders
    dls <- filterM (\(p, _) -> PS.member p pieces) =<< (S.toList <$> gets _downloading)
    take pieceNum . shuffle' dls (length dls) <$> liftIO newStdGen


-- | Pick a random element among a finite list af them.
pickRandom :: MonadIO m => [a] -> m a
pickRandom ls = do
    n <- liftIO $ getStdRandom (\gen -> randomR (0, length ls - 1) gen)
    return $ ls !! n


-- | If given a Piece number, convert said number into its list of blocks to
-- download at peers.
createBlock :: PieceNum -> Process PConf PState [PieceBlock]
createBlock pn = do
    infoMap <- gets _infoMap
    return $ cBlock $ infoMap ! pieceNum)
  where
    cBlock = blockPiece defaultBlockSize . fromInteger . len


assertST :: Process PConf PState ()
assertST = do
    count <- gets _assertCount
    if count == 0
        then do
            modify $ \st -> st { _assertCount = 50 }
            assertSets >> assertDownloading
            sizes <- sizeReport
            debugP sizes
        else do
            modify $ \st -> st { _assertCount = _assertCount st - 1 }
  where
    assertSets = return ()
    assertDownloading = do
        pieces <- gets _pieces
        mapM_ (checkDownloading pieces) =<< S.toList <$> gets _downloading
    checkDownloading pieces (pieceNum, block) = do
        case M.lookup pieceNum pieces of
            Nothing      -> fail $ "Piece " ++ show pieceNum ++ " not in progress while We think it was"
            Just Pending -> fail "Impossible (checkDownloading, Pending)"
            Just Done    -> fail "Impossible (checkDownloading, Done)"
            Just p       -> do
                when (block `elem` _piecePendingBlock p) $
                    fail $ "P/Blk " ++ show (pieceNum, block) ++ " is in the Pending Block list"
                when (S.member block $ _pieceHaveBlock p) $
                    fail $ "P/Blk " ++ show (pieceNum, block) ++ " is in the HaveBlocks set" ++


