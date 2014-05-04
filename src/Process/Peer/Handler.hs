module Process.Peer.Handler
    ( runPeerHandler
    ) where


import Data.Bits (testBit)
import Data.Word (Word8)
import Data.Array
import Data.Maybe (catMaybes)
import qualified Data.ByteString as B
import qualified Data.Set as S
import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader (asks)


import qualified Data.PieceSet as PS
import Process
import Torrent hiding (Piece)
import Torrent.Message (Message(..), Handshake)

import qualified Process.PieceManager as PieceManager


data PConf = PConf
    { _sendChan   :: TChan Message
    , _fromChan   :: TChan (Either Handshake Message)
    , _pieceMChan :: TChan PieceManager.PieceManagerMessage
    , _haveV      :: TMVar [PieceNum]
    , _blockV     :: TMVar PieceManager.PieceMBlock
    , _pieceArray :: PieceArray
    }

instance ProcessName PConf where
    processName _ = "Peer.Handler"

data PState = PState
    { _weChoke           :: Bool
    , _peerChoke         :: Bool
    , _weInterested      :: Bool
    , _peerInterested    :: Bool
    , _isEndgame         :: Bool
    , _peerPieces        :: PS.PieceSet
    , _missingPieces     :: Int
    , _blockQueue        :: S.Set (PieceNum, PieceBlock)
    , _interestPieces    :: S.Set PieceNum
    }

mkState :: PState
mkState = PState
    { _weChoke        = True
    , _peerChoke      = True
    , _weInterested   = False
    , _peerInterested = False
    , _isEndgame      = False
    , _peerPieces     = undefined
    , _missingPieces  = undefined
    , _blockQueue     = undefined
    , _interestPieces = undefined
    }


runPeerHandler :: TChan (Either Handshake Message) -> TChan Message -> IO ()
runPeerHandler fromChan sendChan = do
    let pconf = PConf sendChan fromChan undefined undefined undefined undefined
        pstate = mkState
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState (Either Handshake Message)
wait = do
    fromChan <- asks _fromChan
    liftIO . atomically $ readTChan fromChan


receive :: Either Handshake Message -> Process PConf PState ()
receive (Left handshake) = do
    liftIO . print $ handshake
    return ()
receive (Right message) = do
    liftIO . print $ message
    -- handleMessage message


handleMessage :: Message -> Process PConf PState ()
handleMessage message = do
    case message of
        KeepAlive
            -> return ()
        Choke
            -> handleMessageChoke
        Unchoke
            -> modify (\s -> s { _peerChoke = False })
        Interested
            -> modify (\s -> s { _peerInterested = True  })
        NotInterested
            -> modify (\s -> s { _peerInterested = False })
        Have piecenum
            -> handleMessageHave piecenum
        BitField bitfield
            -> handleMessageBitField bitfield
        Request piecenum block
            -> handleMessageRequest piecenum block
        Piece piecenum offset bs
            -> handleMessagePiece piecenum offset bs
        Cancel piecenum block
            -> handleMessageCancel piecenum block
        Port _ -> return () -- No DHT yet, ignore


handleMessageChoke = do
    putbackBlocks
    modify (\s -> s { _peerChoke = True  })


handleMessageHave pieceNum = do
    pieceArray <- asks _pieceArray
    let (lo, hi) = bounds pieceArray
    if pieceNum >= lo && pieceNum <= hi
        then do
            debugP $ "peer has piece #" ++ show pieceNum
            peerPieces <- gets _peerPieces
            PS.have pieceNum peerPieces
            trackInterestAdd [pieceNum]
            decMissingCounter 1
            fillBlocks
        else do
            errorP $ "unknown piece #" ++ show pieceNum
            stopProcess


handleMessageBitField :: B.ByteString -> Process PConf PState ()
handleMessageBitField bitfield = do
    pieceSet   <- gets _peerPieces
    piecesNull <- PS.null pieceSet
    if piecesNull
        then do
            nPieces    <- succ . snd . bounds <$> asks _pieceArray
            peerPieces <- createPeerPieces nPieces bitfield
            pieceList  <- PS.toList peerPieces
            trackInterestAdd pieceList
            decMissingCounter (length pieceList)
            modify $ \st -> st { _peerPieces = peerPieces }
        else do
            errorP "got out of band Bitfield request, dying"
            stopProcess


handleMessageRequest :: PieceNum -> PieceBlock -> Process PConf PState ()
handleMessageRequest pieceNum block = do
    choking <- gets _weChoke
    unless choking $ do
        debugP $ "Peer requested: " ++ show pieceNum ++ "(" ++ show block ++ ")"
        outChan $ SenderQPiece pieceNum block


handleMessagePiece :: PieceNum -> Integer -> B.ByteString -> Process PConf PState ()
handleMessagePiece pieceNum offset bs = do
    let size  = fromIntegral $ B.length bs
        block = PieceBlock offset size
        record = (pieceNum, block)
    blockQueue <- gets _blockQueue
    if S.member record blockQueue
        then do
            storeBlock pieceNum block bs
            modify $ \st -> st { _blockQueue = S.delete record (_blockQueue st) }
        else
            return ()


handleMessageCancel :: PieceNum -> PieceBlock -> Process PConf PState ()
handleMessageCancel pieceNum block = do
    outChan $ SenderQCancel pieceNum block


trackInterestAdd :: [PieceNum] -> Process PConf PState ()
trackInterestAdd pieceNumList = do
    haveV       <- asks _haveV
    interestSet <- gets _interestPieces
    askPieceManager $ PieceManager.PeerHave pieceNumList haveV
    interestSlice <- liftIO . atomically $ takeTMVar haveV
    let interestSet' = updateInterestSet interestSlice interestSet
    if S.null interestSet
        then do
            modify $ \st -> st { _interestPieces = S.empty }
        else do
            debugP "we are interested"
            modify $ \st -> st { _interestPieces = interestSet'
                               , _weInterested = True
                               }
            -- outChan $ SenderQM Interested
  where
    updateInterestSet slice set = foldl (flip S.insert) set slice


decMissingCounter :: Int -> Process PConf PState ()
decMissingCounter n = do
    missingPieces <- gets _missingPieces
    modify $ \st -> st { _missingPieces = missingPieces - n }


fillBlocks :: Process PConf PState ()
fillBlocks = do
    choked     <- gets _peerChoke
    interested <- gets _weInterested
    when (not choked && interested) checkWatermark


checkWatermark :: Process PConf PState ()
checkWatermark = do
    queue   <- gets _blockQueue
    endgame <- gets _isEndgame
    let size = S.size queue
        mark = if endgame then endgameLoMark else loMark
    when (size < mark) $ do
        toQueue <- grabBlocks (hiMark - size)
        queuePieces toQueue


loMark :: Int
loMark = 5

hiMark :: Int
hiMark = 25

endgameLoMark :: Int
endgameLoMark = 1

storeBlock :: PieceNum -> PieceBlock -> B.ByteString -> Process PConf PState ()
storeBlock pieceNum block bs = askPieceManager $ PieceManager.StoreBlock pieceNum block bs


grabBlocks :: Int -> Process PConf PState [(PieceNum, PieceBlock)]
grabBlocks k = do
    blockV   <- asks _blockV
    pieceSet <- gets _peerPieces
    askPieceManager (PieceManager.GrabBlocks k pieceSet blockV)
    blocks   <- liftIO . atomically $ takeTMVar blockV
    case blocks of
        PieceManager.Leech blocks'   ->
            return blocks'
        PieceManager.Endgame blocks' -> do
            modify $ \st -> st { _isEndgame = True }
            return blocks'


queuePieces :: [(PieceNum, PieceBlock)] -> Process PConf PState ()
queuePieces toQueue = do
    blockQueue <- gets _blockQueue
    toQueueFiltered <- forM toQueue $ \(piece, block) -> do
            if S.member (piece, block) blockQueue
                then return Nothing
                else do
                    outChan $ SenderQM $ Request piece block
                    return $ Just (piece, block)
    modify $ \st -> st { _blockQueue = S.union blockQueue (S.fromList $ catMaybes toQueueFiltered) }


putbackBlocks :: Process PConf PState ()
putbackBlocks = do
    blockQueue <- gets _blockQueue
    askPieceManager $ PieceManager.PutbackBlock $ S.toList blockQueue
    modify $ \st -> st { _blockQueue = S.empty }


askPieceManager :: PieceManager.PieceManagerMessage -> Process PConf PState ()
askPieceManager message = do
   pieceMChan <- asks _pieceMChan
   liftIO . atomically $ writeTChan pieceMChan message


createPeerPieces :: MonadIO m => Integer -> B.ByteString -> m PS.PieceSet
createPeerPieces nPieces =
    PS.fromList nPieces . map fromIntegral . concat . decodeBytes 0 . B.unpack
  where
    decodeByte :: Int -> Word8 -> [Maybe Int]
    decodeByte soFar w =
        let dBit n = if testBit w (7-n)
                        then Just (n+soFar)
                        else Nothing
         in fmap dBit [0..7]
    decodeBytes _ [] = []
    decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws


data SenderQM a
    = SenderQM a
    | SenderQPiece PieceNum PieceBlock
    | SenderQCancel PieceNum PieceBlock
outChan = undefined 
