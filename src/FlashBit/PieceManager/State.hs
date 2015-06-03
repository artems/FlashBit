{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.PieceManager.State
    ( PieceManagerState(..)
    , PieceState(..)
    , mkPieceManagerState
    , getDonePieces
    , getInProgressPieces
    , isPendingPiece
    , putbackPiece
    , putbackBlock
    , storeBlock
    , markPieceDone
    , checkTorrentCompletion
    , pieceLength
    , markPeerHave
    , grabBlocks
    ) where

import Control.Monad.Trans (liftIO, MonadIO)
import qualified Control.Monad.State as S
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.PieceHistogram as H
import System.Random
import System.Random.Shuffle

import Torrent


data PieceManagerState = PieceManagerState
    { _pieceArray   :: PieceArray                   -- ^ Information about pieces
    , _histogram    :: H.PieceHistogram             -- ^ Track the rarity of pieces
    , _pieces       :: M.Map PieceNum PieceState    -- ^ Pieces in progress
    , _blocks       :: S.Set (PieceNum, PieceBlock) -- ^ Blocks we are currently downloading
    , _isEndgame    :: Bool                         -- ^ If we have done any endgame work this is true
    }

data PieceState
    = PieceDone
    | PiecePending
    | PieceInProgress
        { _pieceDone         :: Int              -- ^ Number of blocks when piece is done
        , _pieceHaveBlock    :: S.Set PieceBlock -- ^ The blocks we have
        , _piecePendingBlock :: [PieceBlock]     -- ^ Blocks still pending
        } deriving (Show)

type PieceManagerMonad a = (MonadIO m, S.MonadState PieceManagerState m) => m a


mkPieceManagerState :: PieceArray -> PieceHaveMap -> PieceManagerState
mkPieceManagerState pieceArray pieceHaveMap =
    let pieces = M.map isDone pieceHaveMap
    in
        PieceManagerState
            { _histogram    = H.empty
            , _pieceArray   = pieceArray
            , _pieces       = pieces
            , _blocks       = S.empty
            , _isEndgame    = False
            }
  where
    isDone True  = PieceDone
    isDone False = PiecePending


filterDone :: PieceState -> Bool
filterDone PieceDone = True
filterDone _         = False

filterPending :: PieceState -> Bool
filterPending PiecePending = True
filterPending _            = False

filterInProgress :: PieceState -> Bool
filterInProgress (PieceInProgress _ _ _) = True
filterInProgress _                       = False


getDonePieces :: PieceManagerMonad [PieceNum]
getDonePieces = do
    pieces <- S.gets _pieces
    return $ (M.keys . M.filter filterDone) pieces


getInProgressPieces :: PieceManagerMonad [PieceNum]
getInProgressPieces = do
    pieces <- S.gets _pieces
    return $ (M.keys . M.filter filterInProgress) pieces


isPendingPiece :: PieceNum -> M.Map PieceNum PieceState -> Bool
isPendingPiece pieceNum pieces =
    case M.lookup pieceNum pieces of
        Just PiecePending -> True
        _                 -> False


markPeerHave :: [PieceNum] -> PieceManagerMonad [PieceNum]
markPeerHave pieceNum = do
    pieces    <- S.gets _pieces
    histogram <- S.gets _histogram
    let interested = filter (filterInterested pieces) pieceNum
    S.when (not . null $ interested) $ do
        S.modify $ \s -> s { _histogram = H.haveAll interested histogram }
    return interested
  where
    filterInterested pieces pieceNum' =
        case M.lookup pieceNum' pieces of
            Nothing        -> False
            Just PieceDone -> False
            Just _         -> True


putbackBlock :: (PieceNum, PieceBlock) -> PieceManagerMonad ()
putbackBlock (pieceNum, block) = do
    pieces <- S.gets _pieces
    blocks <- S.gets _blocks
    case M.lookup pieceNum pieces of
        Nothing             ->
            error "putbackBlock: not found"
        Just PiecePending   ->
            error "putbackBlock: pending"
        Just PieceDone      ->
            return () -- stray block at endgame
        Just piece          -> do
            S.modify $ \s -> s
                { _pieces = M.insert pieceNum (update piece) pieces
                , _blocks = S.delete (pieceNum, block) blocks
                }
  where
    update piece
        | S.member block (_pieceHaveBlock piece) = piece
        | otherwise = piece { _piecePendingBlock = block : _piecePendingBlock piece }


putbackPiece :: PieceNum -> PieceManagerMonad ()
putbackPiece pieceNum =
    S.modify $ \s -> s { _pieces = M.alter setPending pieceNum (_pieces s) }
  where
    setPending (Just (PieceInProgress _ _ _)) = Just PiecePending
    setPending _ = error "putbackPiece: not in progress"


storeBlock :: PieceNum -> PieceBlock -> PieceManagerMonad Bool
storeBlock pieceNum block = do
    S.modify $ \s -> s { _blocks = S.delete (pieceNum, block) (_blocks s) }
    updateProgress pieceNum block


updateProgress :: PieceNum -> PieceBlock -> PieceManagerMonad Bool
updateProgress pieceNum block = do
    pieces <- S.gets _pieces
    case M.lookup pieceNum pieces of
        Nothing             ->
            error "updateProgress: piece not found"
        Just PiecePending   ->
            error "updateProgress: pending"
        Just PieceDone      ->
            return False -- This happens when a stray block is downloaded
        Just piece          ->
            let blockSet = _pieceHaveBlock piece
            in
                if block `S.member` blockSet
                    then return False
                    else do
                        let piece' = piece { _pieceHaveBlock = S.insert block blockSet }
                        S.modify $ \s -> s { _pieces = M.insert pieceNum piece' pieces }
                        return $ pieceHave piece' == _pieceDone piece'
  where
    pieceHave = S.size . _pieceHaveBlock


markPieceDone :: PieceNum -> PieceManagerMonad Bool
markPieceDone pieceNum = do
    completePiece pieceNum
    checkTorrentCompletion


completePiece :: PieceNum -> PieceManagerMonad ()
completePiece pieceNum = do
    S.modify $ \s -> s
        { _pieces    = M.update setDone pieceNum (_pieces s)
        , _histogram = H.remove pieceNum (_histogram s)
        }
  where
    setDone (PieceInProgress _ _ _) = Just PieceDone
    setDone _                       = error "completePiece: impossible"


checkTorrentCompletion :: PieceManagerMonad Bool
checkTorrentCompletion = do
    pieces     <- S.gets _pieces
    pieceArray <- S.gets _pieceArray
    let totalPieces = pieceArraySize pieceArray
    return $ totalPieces == countDonePieces pieces
  where
    countDonePieces :: M.Map PieceNum PieceState -> Integer
    countDonePieces = fromIntegral . M.size . M.filter filterDone


createPieceBlocks :: PieceNum -> PieceManagerMonad [PieceBlock]
createPieceBlocks pieceNum = do
    pieceSize <- pieceLength pieceNum
    return $ splitPieceIntoBlocks defaultBlockSize pieceSize


splitPieceIntoBlocks :: PieceBlockLength -> PieceSize -> [PieceBlock]
splitPieceIntoBlocks blockSize pieceSize = build pieceSize 0 []
  where
    build 0 _offset acc = reverse acc

    build leftBytes offset acc
        | leftBytes >= blockSize =
            let leftBytes' = leftBytes - blockSize
                offset'    = offset + blockSize
                block      = PieceBlock offset blockSize
            in
                build leftBytes' offset' (block : acc)
        | otherwise = build 0 (offset + leftBytes) (PieceBlock offset leftBytes : acc)


pieceLength :: PieceNum -> PieceManagerMonad Integer
pieceLength pieceNum = do
    pieceArray <- S.gets _pieceArray
    let piece = pieceArray A.! pieceNum
    return $ fromInteger (_pieceLength piece)


grabBlocks :: Integer -> S.Set PieceNum -> PieceManagerMonad (TorrentPieceMode, [(PieceNum, PieceBlock)])
grabBlocks num pieceSet = do
    pieces <- S.gets _pieces
    blocks <- tryGrab num pieceSet
    let pending = M.filter filterPending pieces
    if null blocks && M.null pending
        then do
            S.modify $ \st -> st { _isEndgame = True }
            blocks' <- grabEndGame num pieceSet
            return (Endgame, blocks')
        else do
            S.modify $ \st -> st { _blocks = foldl' (flip S.insert) (_blocks st) blocks }
            return (Leech, blocks)


tryGrab :: Integer -> S.Set PieceNum -> PieceManagerMonad [(PieceNum, PieceBlock)]
tryGrab num pieceSet = do
    inProgress <- getInProgressPieces
    tryGrabProgress num pieceSet [] inProgress


tryGrabProgress :: Integer -> S.Set PieceNum -> [(PieceNum, PieceBlock)] -> [PieceNum]
                -> PieceManagerMonad [(PieceNum, PieceBlock)]
tryGrabProgress 0   _        captured _  = return captured
tryGrabProgress num pieceSet captured [] = tryGrabPending num pieceSet captured
tryGrabProgress num pieceSet captured (pieceNum : xs)
    | pieceNum `S.member` pieceSet = grabFromProgress num pieceSet pieceNum captured xs
    | otherwise                    = tryGrabProgress num pieceSet captured xs


grabFromProgress :: Integer -> S.Set PieceNum -> PieceNum -> [(PieceNum, PieceBlock)] -> [PieceNum]
                 -> PieceManagerMonad [(PieceNum, PieceBlock)]
grabFromProgress num pieceSet pieceNum captured xs = do
    pieces <- S.gets _pieces
    piece  <- case M.lookup pieceNum pieces of
        Nothing           -> error "grabFromProgress: not found"
        Just PieceDone    -> error "grabFromProgress: done"
        Just PiecePending -> error "grabFromProgress: pending"
        Just x            -> return x
    let (grabbed, rest) = splitAt (fromIntegral num) (_piecePendingBlock piece)
        piece' = piece { _piecePendingBlock = rest }

    if null grabbed
        then tryGrabProgress num pieceSet captured xs
        else do
            let num' = num - (fromIntegral $ length grabbed)
                captured' = [(pieceNum, block) | block <- grabbed]
            S.modify $ \st -> st { _pieces = M.insert pieceNum piece' pieces }
            tryGrabProgress num' pieceSet (captured' ++ captured) xs


tryGrabPending :: Integer -> S.Set PieceNum -> [(PieceNum, PieceBlock)]
               -> PieceManagerMonad [(PieceNum, PieceBlock)]
tryGrabPending num pieceSet captured = do
    pieces     <- S.gets _pieces
    histogram  <- S.gets _histogram
    let culprits = H.pick (predicate pieces) 10 histogram
    case culprits of
        []  -> return captured
        pending -> do
            pieceNum  <- pickRandom pending
            blockList <- createPieceBlocks pieceNum
            let size  = length blockList
                piece = PieceInProgress size S.empty blockList
            S.modify $ \st -> st { _pieces = M.insert pieceNum piece pieces }
            tryGrabProgress num pieceSet captured [pieceNum]
  where
    predicate pieces pieceNum =
        let member  = pieceNum `S.member` pieceSet
            pending = isPendingPiece pieceNum pieces
        in
            member && pending


grabEndGame :: Integer -> S.Set PieceNum -> PieceManagerMonad [(PieceNum, PieceBlock)]
grabEndGame num pieceSet = do
    gen <- liftIO newStdGen
    blocks <- S.gets _blocks
    let downloading = filter peerHavePiece (S.toList blocks)
    return $ take (fromIntegral num) $ shuffle' downloading (length downloading) gen
  where
    peerHavePiece (pieceNum, _) = pieceNum `S.member` pieceSet


pickRandom :: MonadIO m => [a] -> m a
pickRandom xs = do
    index <- liftIO $ randomRIO (0, length xs - 1)
    return $ xs !! index
