module Process.PieceManager
    ( runPieceManager
    , PieceManagerMessage(..)
    , PieceMBlock(..)
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when, unless, filterM)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)


import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.PieceSet as PS
import qualified Data.PieceHistogram as PH
import qualified Data.ByteString as B

import System.Random
import System.Random.Shuffle

import Torrent
import Process
import Process.Status
import Process.FileAgent
import Process.ChokeManager



data PieceManagerMessage
    = GrabBlocks Int PS.PieceSet (TMVar PieceMBlock)
      -- ^ Ask for grabbing some blocks
    | PutbackBlock [(PieceNum, PieceBlock)]
      -- ^ Put these blocks back for retrieval
    | StoreBlock PieceNum PieceBlock B.ByteString
      -- ^ Ask for storing a block on the file system
    | GetDone (TMVar [PieceNum])
      -- ^ Get the pieces which are already done
    | PeerHave [PieceNum] (TMVar [PieceNum])
      -- ^ A peer has the given piece(s)
    | PeerUnhave [PieceNum]
      -- ^ A peer relinquished the given piece Indexes


data PieceMBlock
    = Leech [(PieceNum, PieceBlock)]
    | Endgame [(PieceNum, PieceBlock)]


instance Show PieceManagerMessage where
    show (GrabBlocks x _ _)   = "GrabBlocks " ++ show x
    show (PutbackBlock x)     = "PutbackBlock " ++ show x
    show (StoreBlock pieceNum block _) =
        "StoreBlock " ++ show pieceNum ++ " " ++ show block
    show (GetDone _)          = "GetDone"
    show (PeerHave pieces _)  = "PeerHave " ++ show pieces
    show (PeerUnhave pieces)  = "PeerUnhave " ++ show pieces


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
    { _donePush      :: [ChokeManagerMessage]        -- ^ Pieces that should be pushed to the Choke Mgr.
    , _downPieces    :: M.Map PieceNum PieceState    -- ^ Pieces in progress
    , _downBlocks    :: S.Set (PieceNum, PieceBlock) -- ^ Blocks we are currently downloading
    , _pieceArray    :: PieceArray                   -- ^ Information about pieces
    , _histogram     :: PH.PieceHistogram            -- ^ Track the rarity of pieces
    , _endGaming     :: Bool                         -- ^ If we have done any endgame work this is true
    , _assertCount   :: Int                          -- ^ When to next check the database for consistency
    }


data PieceState
    = PieceDone
    | PiecePending
    | PieceInProgress
        { _pieceDone         :: Int              -- ^ Number of blocks when piece is done
        , _pieceHaveBlock    :: S.Set PieceBlock -- ^ The blocks we have
        , _piecePendingBlock :: [PieceBlock]     -- ^ Blocks still pending
        } deriving (Show)


runPieceManager :: InfoHash -> PieceArray -> PieceHaveMap
    -> TChan PieceManagerMessage
    -> TChan FileAgentMessage
    -> TChan StatusMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPieceManager infohash pieceArray pieceHaveMap pieceMChan fsChan statusChan chokeMChan = do
    let pconf  = PConf infohash pieceMChan fsChan statusChan chokeMChan
        pstate = createPieceDb pieceHaveMap pieceArray
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    drainSend
    process


wait :: Process PConf PState PieceManagerMessage
wait = do
    pieceMChan <- asks _pieceMChan
    liftIO . atomically $ readTChan pieceMChan


receive :: PieceManagerMessage -> Process PConf PState ()
receive message = do
    case message of
        GrabBlocks num eligible blockV -> do
            blocks <- grabBlocks num eligible
            liftIO . atomically $ putTMVar blockV blocks

        PutbackBlock blocks ->
            mapM_ putbackBlock blocks

        StoreBlock pieceNum block pieceData ->
            storeBlock pieceNum block pieceData

        GetDone doneV -> do
            pieces <- gets _downPieces
            liftIO . atomically $ putTMVar doneV (donePieces pieces)
          where
            donePieces = M.keys . M.filter f
              where
                f PieceDone = True
                f _         = False

        PeerHave pieces interestV ->
            peerHave pieces interestV

        PeerUnhave pieces ->
            peerUnhave pieces


isPendingPiece :: PieceNum -> M.Map PieceNum PieceState -> Bool
isPendingPiece pieceNum pieceMap =
    case M.lookup pieceNum pieceMap of
        Just PiecePending -> True
        _                 -> False


inProgressPieces :: M.Map PieceNum PieceState -> [PieceNum]
inProgressPieces pieceMap = M.keys $ M.filter f pieceMap
  where
    f (PieceInProgress {}) = True
    f _                    = False



createPieceDb :: PieceHaveMap -> PieceArray -> PState
createPieceDb pieceHaveMap pieceArray =
    PState [] (M.map f pieceHaveMap) S.empty pieceArray PH.empty False 0
  where
    f True  = PieceDone
    f False = PiecePending


drainSend :: Process PConf PState ()
drainSend = do
    donePush <- gets _donePush
    unless (null donePush) $ do
        chokeMChan <- asks _chokeMChan
        liftIO . atomically $ writeTChan chokeMChan (head donePush)
        modify $ \st -> st { _donePush = tail (_donePush st) }


peerHave :: [PieceNum] -> TMVar [PieceNum] -> Process PConf PState ()
peerHave pieces interestV = do
    downPieces <- gets _downPieces
    let interesting = filter (f downPieces) pieces
    liftIO . atomically $ putTMVar interestV interesting
    unless (null interesting) $ do
        modify $ \st -> st { _histogram = PH.allHave interesting (_histogram st) }
  where
    f pieces pieceNum = case M.lookup pieceNum pieces of
        Nothing        -> False
        Just PieceDone -> False
        Just _         -> True


peerUnhave :: [PieceNum] -> Process PConf PState ()
peerUnhave pieces = do
    modify $ \st -> st { _histogram = PH.allUnhave pieces (_histogram st) }


putbackBlock :: (PieceNum, PieceBlock) -> Process PConf PState ()
putbackBlock (pieceNum, block) = do
    pieces <- gets _downPieces
    case M.lookup pieceNum pieces of
        Nothing
            -> fail "putbackBlock: wrong pieceNum"
        Just PiecePending
            -> fail "putbackBlock: perhaps stray pieceNum"
        Just PieceDone
            -> return () -- stray block at endgame
        Just piece -> do
            modify $ \st -> st { _downPieces = M.insert pieceNum (update piece) pieces
                               , _downBlocks = S.delete (pieceNum, block) (_downBlocks st)
                               }
  where
    update piece
        | S.member block (_pieceHaveBlock piece) = piece
        | otherwise = piece { _piecePendingBlock = block : _piecePendingBlock piece }


storeBlock :: PieceNum -> PieceBlock -> B.ByteString -> Process PConf PState ()
storeBlock pieceNum block pieceData = do
    fsChan <- asks _fsChan
    liftIO . atomically $ writeTChan fsChan $ WriteBlock pieceNum block pieceData
    modify $ \st -> st { _downBlocks = S.delete (pieceNum, block) (_downBlocks st) }

    endgameBroadcast pieceNum block
    done <- updateProgress pieceNum block
    when done $ pieceDone pieceNum


endgameBroadcast :: PieceNum -> PieceBlock -> Process PConf PState ()
endgameBroadcast pieceNum block = do
    infohash  <- asks _infoHash
    endGaming <- gets _endGaming
    when endGaming $ do
        pieces <- gets _donePush
        let pieces' = ChokeMBlockComplete infohash pieceNum block : pieces
        modify $ \st -> st { _donePush = pieces' }


updateProgress :: PieceNum -> PieceBlock -> Process PConf PState Bool
updateProgress pieceNum block = do
    pieces <- gets _downPieces
    case M.lookup pieceNum pieces of
        Nothing           -> fail "updateProgress: wrong piece_num"
        Just PiecePending -> fail "updateProgress: pending"
        Just PieceDone    -> return False -- This happens when a stray block is downloaded
                     -- TODO: Consider handling it elsewhere in the stack
        Just piece ->
            let blockSet = _pieceHaveBlock piece
            in  if block `S.member` blockSet
                    then return False -- Stray block download.
                                      -- Will happen without FAST extension
                                      -- at times
                    else do
                        let piece' = piece { _pieceHaveBlock = S.insert block blockSet }
                        modify $ \st -> st { _downPieces = M.insert pieceNum piece' (_downPieces st) }
                        return (pieceHave piece' == _pieceDone piece')
  where
    pieceHave = S.size . _pieceHaveBlock


pieceDone :: PieceNum -> Process PConf PState ()
pieceDone pieceNum = do
    debugP $ "Marking piece #" ++ show pieceNum ++ " done"
    pieceOk <- checkPiece pieceNum
    case pieceOk of
        True -> do
            infohash   <- asks _infoHash
            statusChan <- asks _statusChan
            markDone pieceNum
            completePiece pieceNum
            checkFullCompletion
            pieceArray <- gets _pieceArray
            let len = _pieceLength $ pieceArray A.! pieceNum
            liftIO . atomically $ writeTChan statusChan $
                CompletedPiece infohash len

        False ->
            putbackPiece pieceNum


checkPiece :: PieceNum -> Process PConf PState Bool
checkPiece pieceNum = do
    fsChan <- asks _fsChan
    checkV <- liftIO newEmptyTMVarIO
    liftIO $ do
        atomically $ writeTChan fsChan $ CheckPiece pieceNum checkV
        atomically $ takeTMVar checkV


completePiece :: PieceNum -> Process PConf PState ()
completePiece pieceNum = do
    modify $ \st -> st { _downPieces = M.update f pieceNum (_downPieces st)
                       , _histogram  = PH.remove pieceNum (_histogram st)
                       }
  where
    f (PieceInProgress _ _ _) = Just PieceDone
    f _                       = error "completePiece: impossible"


markDone :: PieceNum -> Process PConf PState ()
markDone pieceNum = do
    infohash <- asks _infoHash
    modify $ \st -> st { _donePush = ChokeMPieceDone infohash pieceNum : _donePush st }


checkFullCompletion :: Process PConf PState ()
checkFullCompletion = do
    pieces <- gets _downPieces
    pieceArray <- gets _pieceArray
    when (succ (snd (A.bounds pieceArray)) == countDonePieces pieces) $ do
        infoP "Torrent Completed; to honor the torrent-gods thou must now sacrifice a goat!"
        infohash   <- asks _infoHash
        chokeMChan <- asks _chokeMChan
        statusChan <- asks _statusChan
        liftIO . atomically $ writeTChan statusChan $ TorrentCompleted infohash
        liftIO . atomically $ writeTChan chokeMChan $ ChokeMTorrentComplete infohash


countDonePieces :: M.Map PieceNum PieceState -> Integer
countDonePieces = fromIntegral . M.size . M.filter f
  where
    f PieceDone = True
    f _         = False


putbackPiece :: PieceNum -> Process PConf PState ()
putbackPiece pieceNum = do
    modify $ \st -> st { _downPieces = M.alter f pieceNum (_downPieces st) }
  where
    f (Just (PieceInProgress _ _ _)) = Just PiecePending
    f _                              = error "putbackPiece: impossible"



grabBlocks :: Int -> PS.PieceSet -> Process PConf PState PieceMBlock
grabBlocks k eligible = do
    pieces <- gets _downPieces
    blocks <- tryGrab k eligible
    let pendingNone = M.null $ M.filter f pieces
    if blocks == [] && pendingNone
        then do
            blocks' <- grabEndGame k eligible
            modify $ \st -> st { _endGaming = True }
            return $ Endgame blocks'
        else do
            modify $ \st -> st { _downBlocks = foldl' (flip S.insert) (_downBlocks st) blocks }
            return $ Leech blocks
  where
    f PiecePending = True
    f _            = False



tryGrab :: Int -> PS.PieceSet -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrab k pieceSet = do
    pieces <- gets _downPieces
    let inProgress = inProgressPieces pieces
    tryGrabProgress k pieceSet [] inProgress


tryGrabProgress :: Int -> PS.PieceSet -> [(PieceNum, PieceBlock)] -> [PieceNum]
                -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrabProgress 0 _        captured _  = return captured
tryGrabProgress k pieceSet captured [] = tryGrabPending k pieceSet captured
tryGrabProgress k pieceSet captured (pieceNum : xs) = do
    pieceExists <- PS.exists pieceNum pieceSet
    if pieceExists
        then grabFromProgress k pieceSet pieceNum captured xs
        else tryGrabProgress k pieceSet captured xs


grabFromProgress :: Int -> PS.PieceSet -> PieceNum -> [(PieceNum, PieceBlock)] -> [PieceNum]
                 -> Process PConf PState [(PieceNum, PieceBlock)]
grabFromProgress k ps pieceNum captured xs = do
    pieces <- gets _downPieces
    gpiece <- case M.lookup pieceNum pieces of
              Nothing           -> fail "grabFromProgress: could not lookup piece"
              Just PieceDone    -> fail "Impossible (Done, grabFromProgress)"
              Just PiecePending -> fail "Impossible (Pending, grabFromProgress)"
              Just x            -> return x
    let (grabbed, rest) = splitAt k (_piecePendingBlock gpiece)
        gpiece' = gpiece { _piecePendingBlock = rest }

    if null grabbed
        then tryGrabProgress k ps captured xs
        else do
            let k' = k - length grabbed
                captured' = [(pieceNum, g) | g <- grabbed]
            modify $ \st -> st { _downPieces = M.insert pieceNum gpiece' pieces }
            tryGrabProgress k' ps (captured' ++ captured) xs


tryGrabPending :: Int -> PS.PieceSet -> [(PieceNum, PieceBlock)]
               -> Process PConf PState [(PieceNum, PieceBlock)]
tryGrabPending k pieceSet captured = do
    pieces     <- gets _downPieces
    histogram  <- gets _histogram
    pieceSet'  <- PS.freeze pieceSet
    let pred pieceNum =
            let pend = isPendingPiece pieceNum pieces
                mem  = PS.exists' pieceNum pieceSet'
            in  (pend && mem)
    let culprits = PH.pick pred 7 histogram
    case culprits of
        []  -> return captured
        ps' -> do
            pieceNum <- pickRandom ps'
            blockList <- createBlock pieceNum
            let size = length blockList
                piece = PieceInProgress size S.empty blockList
            modify $ \st -> st { _downPieces = M.insert pieceNum piece pieces }
            tryGrabProgress k pieceSet captured [pieceNum]


pickRandom :: MonadIO m => [a] -> m a
pickRandom xs = do
    index <- liftIO $ randomRIO (0, length xs - 1)
    return $ xs !! index


createBlock :: PieceNum -> Process PConf PState [PieceBlock]
createBlock pieceNum = do
    pieceArray <- gets _pieceArray
    return $ cBlock $ pieceArray A.! pieceNum
  where
    cBlock = blockPiece defaultBlockSize . fromInteger . _pieceLength


blockPiece :: PieceBlockSize -> PieceSize -> [PieceBlock]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where
    build 0        _os acc = reverse acc
    build leftBytes os acc
        | leftBytes >= blockSz =
            build (leftBytes - blockSz) (os + blockSz) $ PieceBlock os blockSz : acc
        | otherwise = build 0 (os + leftBytes) $ PieceBlock os leftBytes : acc


grabEndGame :: Int -> PS.PieceSet -> Process PConf PState [(PieceNum, PieceBlock)]
grabEndGame pieceNum pieceSet = do
    dls <- filterM (\(p, _) -> PS.exists p pieceSet) =<< (S.toList `fmap` gets _downBlocks)
    gen <- liftIO newStdGen
    return $ take pieceNum (shuffle' dls (length dls) gen)


