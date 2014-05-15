module Process.PieceManager
    ( PieceManagerMessage(..)
    , PieceManagerGrabBlockMode(..)
    , runPieceManager
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
    = PieceManagerGrabBlock Int PS.PieceSet
        (TMVar (PieceManagerGrabBlockMode, [(PieceNum, PieceBlock)]))
    | PieceManagerPutbackBlock [(PieceNum, PieceBlock)]
    | PieceManagerStoreBlock PieceNum PieceBlock B.ByteString
    | PieceManagerGetDone (TMVar [PieceNum])
    | PieceManagerPeerHave [PieceNum] (TMVar [PieceNum])
    | PieceManagerPeerUnhave [PieceNum]


data PieceManagerGrabBlockMode = Leech | Endgame


data PConf = PConf
    { _infoHash      :: InfoHash
    , _pieceMChan    :: TChan PieceManagerMessage
    , _fileAgentChan :: TChan FileAgentMessage
    , _statusChan    :: TChan StatusMessage
    , _chokeMChan    :: TChan ChokeManagerMessage
    }


instance ProcessName PConf where
    processName _ = "PieceManager"


data PState = PState
    { _downPieces    :: M.Map PieceNum PieceState    -- ^ Pieces in progress
    , _downBlocks    :: S.Set (PieceNum, PieceBlock) -- ^ Blocks we are currently downloading
    , _pieceArray    :: PieceArray                   -- ^ Information about pieces
    , _histogram     :: PH.PieceHistogram            -- ^ Track the rarity of pieces
    , _isEndgame     :: Bool                         -- ^ If we have done any endgame work this is true
    }


data PieceState
    = PieceDone
    | PiecePending
    | PieceInProgress
        { _pieceDone         :: Int              -- ^ Number of blocks when piece is done
        , _pieceHaveBlock    :: S.Set PieceBlock -- ^ The blocks we have
        , _piecePendingBlock :: [PieceBlock]     -- ^ Blocks still pending
        } deriving (Show)


mkState :: PieceHaveMap -> PieceArray -> PState
mkState pieceHaveMap pieceArray =
    let downPieces = M.map isDone pieceHaveMap
     in PState downPieces S.empty pieceArray PH.empty False
  where
    isDone True  = PieceDone
    isDone False = PiecePending


runPieceManager
    :: InfoHash -> PieceArray -> PieceHaveMap
    -> TChan PieceManagerMessage
    -> TChan FileAgentMessage
    -> TChan StatusMessage
    -> TChan ChokeManagerMessage
    -> IO ()
runPieceManager infohash pieceArray pieceHaveMap pieceMChan fileAgentChan statusChan chokeMChan = do
    let pconf  = PConf infohash pieceMChan fileAgentChan statusChan chokeMChan
        pstate = mkState pieceHaveMap pieceArray
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState PieceManagerMessage
wait = do
    pieceMChan <- asks _pieceMChan
    liftIO . atomically $ readTChan pieceMChan


receive :: PieceManagerMessage -> Process PConf PState ()
receive message = do
    case message of
        PieceManagerGrabBlock num peerPieces blockV -> do
            blocks <- grabBlocks num peerPieces
            liftIO . atomically $ putTMVar blockV blocks

        PieceManagerPutbackBlock blocks ->
            mapM_ putbackBlock blocks

        PieceManagerStoreBlock pieceNum block pieceData ->
            storeBlock pieceNum block pieceData

        PieceManagerGetDone doneV -> do
            pieces <- gets _downPieces
            liftIO . atomically $ putTMVar doneV (donePieces pieces)
          where
            donePieces = M.keys . M.filter f
              where
                f PieceDone = True
                f _         = False

        PieceManagerPeerHave pieces interestV ->
            peerHave pieces interestV

        PieceManagerPeerUnhave pieces ->
            peerUnhave pieces


peerHave :: [PieceNum] -> TMVar [PieceNum] -> Process PConf PState ()
peerHave pieces interestV = do
    downPieces <- gets _downPieces
    let interested = filter (f downPieces) pieces
    liftIO . atomically $ putTMVar interestV interested
    unless (null interested) $ do
        modify $ \st -> st { _histogram = PH.allHave interested (_histogram st) }
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
            -> fail "putbackBlock: wrong piece_num"
        Just PiecePending
            -> fail "putbackBlock: perhaps stray piece_num"
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
    fileAgentChan <- asks _fileAgentChan
    liftIO . atomically $ writeTChan fileAgentChan $ FileAgentWriteBlock pieceNum block pieceData
    modify $ \st -> st { _downBlocks = S.delete (pieceNum, block) (_downBlocks st) }

    done <- updateProgress pieceNum block
    when done $ pieceDone pieceNum


updateProgress :: PieceNum -> PieceBlock -> Process PConf PState Bool
updateProgress pieceNum block = do
    pieces <- gets _downPieces
    case M.lookup pieceNum pieces of
        Nothing           -> fail "updateProgress: wrong piece_num"
        Just PiecePending -> fail "updateProgress: pending"
        Just PieceDone    -> return False -- This happens when a stray block is downloaded
        Just piece        ->
            let blockSet = _pieceHaveBlock piece
            in  if block `S.member` blockSet
                    then do
                        return False
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
            pieceArray <- gets _pieceArray
            markDone pieceNum
            completePiece pieceNum
            checkTorrentCompletion
            let len = _pieceLength $ pieceArray A.! pieceNum
            liftIO . atomically $ writeTChan statusChan $
                StatusCompletedPiece infohash len
        False -> do
            putbackPiece pieceNum


checkPiece :: PieceNum -> Process PConf PState Bool
checkPiece pieceNum = do
    checkV        <- liftIO newEmptyTMVarIO
    fileAgentChan <- asks _fileAgentChan
    liftIO $ do
        atomically $ writeTChan fileAgentChan $ FileAgentCheckPiece pieceNum checkV
        atomically $ takeTMVar checkV


markDone :: PieceNum -> Process PConf PState ()
markDone pieceNum = do
    infoHash <- asks _infoHash
    -- modify $ \st -> st { _donePush = ChokeMPieceDone infohash pieceNum : _donePush st }
    return ()


completePiece :: PieceNum -> Process PConf PState ()
completePiece pieceNum = do
    modify $ \st -> st { _downPieces = M.update f pieceNum (_downPieces st)
                       , _histogram  = PH.remove pieceNum (_histogram st)
                       }
  where
    f (PieceInProgress _ _ _) = Just PieceDone
    f _                       = error "completePiece: impossible"


checkTorrentCompletion :: Process PConf PState ()
checkTorrentCompletion = do
    downPieces <- gets _downPieces
    pieceArray <- gets _pieceArray
    when ((succ . snd . A.bounds $ pieceArray) == countDonePieces downPieces) $ do
        infoP "Torrent Completed; to honor the torrent-gods thou must now sacrifice a goat!"
        infohash   <- asks _infoHash
        chokeMChan <- asks _chokeMChan
        statusChan <- asks _statusChan
        liftIO . atomically $ writeTChan statusChan $ StatusTorrentCompleted infohash
        liftIO . atomically $ writeTChan chokeMChan $ ChokeManagerTorrentComplete infohash


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


grabBlocks :: Int -> PS.PieceSet -> Process PConf PState (PieceManagerGrabBlockMode, [(PieceNum, PieceBlock)])
grabBlocks k peerPieces= do
    pieces <- gets _downPieces
    blocks <- tryGrab k peerPieces
    let pendingNone = M.null $ M.filter f pieces
    if blocks == [] && pendingNone
        then do
            blocks' <- grabEndGame k peerPieces
            modify $ \st -> st { _isEndgame = True }
            return $ (Endgame, blocks')
        else do
            modify $ \st -> st { _downBlocks = foldl' (flip S.insert) (_downBlocks st) blocks }
            return $ (Leech, blocks)
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
            build (leftBytes - blockSz) (os + blockSz) $ PieceBlock blockSz os : acc
        | otherwise = build 0 (os + leftBytes) $ PieceBlock leftBytes os : acc


grabEndGame :: Int -> PS.PieceSet -> Process PConf PState [(PieceNum, PieceBlock)]
grabEndGame pieceNum pieceSet = do
    dls <- filterM (\(p, _) -> PS.exists p pieceSet) =<< (S.toList `fmap` gets _downBlocks)
    gen <- liftIO newStdGen
    return $ take pieceNum (shuffle' dls (length dls) gen)


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


