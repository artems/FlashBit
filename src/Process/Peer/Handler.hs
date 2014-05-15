module Process.Peer.Handler
    ( runPeerHandler
    ) where


import Data.Bits (testBit)
import Data.Word (Word8)
import Data.Time.Clock
import qualified Data.Array as A
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.PieceSet as PS
import qualified Data.ByteString as B

import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Monad (forM, unless, when)
import Control.Monad.Trans (MonadIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks, liftIO)

import qualified Rate as R

import Process
import Process.Channel
import Process.Status
import Process.PieceManager
import Process.Peer.SenderQueue

import Timer
import Torrent
import qualified Torrent.Message as TM


data PConf = PConf
    { _infoHash   :: InfoHash
    , _pieceArray :: PieceArray
    , _numPieces  :: Integer
    , _rateV      :: RateTVar
    , _statV      :: TVar [UpDownStat]
    , _haveV      :: TMVar [PieceNum]
    , _blockV     :: TMVar (PieceManagerGrabBlockMode, [(PieceNum, PieceBlock)])
    , _sendChan   :: TChan SenderQueueMessage
    , _fromChan   :: TChan PeerHandlerMessage
    , _pieceMChan :: TChan PieceManagerMessage
    }

instance ProcessName PConf where
    processName _ = "Peer.Handler"

mkConf :: InfoHash -> PieceArray -> Integer -> RateTVar -> TVar [UpDownStat]
       -> TChan SenderQueueMessage
       -> TChan PeerHandlerMessage
       -> TChan PieceManagerMessage
       -> IO PConf

mkConf infoHash pieceArray numPieces rateV statV sendChan fromChan pieceMChan = do
    haveV  <- newEmptyTMVarIO
    blockV <- newEmptyTMVarIO
    return $ PConf
        { _infoHash   = infoHash
        , _pieceArray = pieceArray
        , _numPieces  = numPieces
        , _rateV      = rateV
        , _statV      = statV
        , _haveV      = haveV
        , _blockV     = blockV
        , _sendChan   = sendChan
        , _fromChan   = fromChan
        , _pieceMChan = pieceMChan
        }


data PState = PState
    { _weChoke           :: Bool
    , _peerChoke         :: Bool
    , _weInterested      :: Bool
    , _peerInterested    :: Bool
    , _isEndgame         :: Bool
    , _blockQueue        :: S.Set (PieceNum, PieceBlock)
    , _peerPieces        :: PS.PieceSet
    , _interestPieces    :: S.Set PieceNum
    , _missingPieces     :: Integer
    , _upRate            :: R.Rate
    , _downRate          :: R.Rate
    }

mkState :: Integer -> IO PState
mkState numPieces = do
    timestamp <- getCurrentTime
    pieceSet  <- PS.new numPieces
    return $ PState
        { _weChoke        = True
        , _peerChoke      = True
        , _weInterested   = False
        , _peerInterested = False
        , _isEndgame      = False
        , _blockQueue     = S.empty
        , _peerPieces     = pieceSet
        , _interestPieces = S.empty
        , _missingPieces  = numPieces
        , _upRate         = R.mkRate timestamp
        , _downRate       = R.mkRate timestamp
        }


runPeerHandler :: InfoHash -> PieceArray -> Integer -> RateTVar -> TVar [UpDownStat]
               -> TChan SenderQueueMessage
               -> TChan PeerHandlerMessage
               -> TChan PieceManagerMessage
               -> IO ()
runPeerHandler infoHash pieceArray numPieces rateV statV sendChan fromChan pieceMChan = do
    pconf  <- mkConf infoHash pieceArray numPieces rateV statV sendChan fromChan pieceMChan
    pstate <- mkState numPieces
    setTimeout 5 $ atomically $ writeTChan fromChan PeerHandlerTick
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState PeerHandlerMessage
wait = do
    fromChan <- asks _fromChan
    liftIO . atomically $ readTChan fromChan


receive :: PeerHandlerMessage -> Process PConf PState ()
receive message = do
    case message of
        PeerHandlerFromPeer fromPeer transferred -> do
            updateDownRate transferred
            case fromPeer of
                Left handshake -> do
                    -- TODO check handshake
                    haveV     <- asks _haveV
                    numPieces <- asks _numPieces
                    askPieceManager $ PieceManagerGetDone haveV
                    completePieces <- liftIO . atomically $ takeTMVar haveV

                    let bitfield = TM.buildBitField numPieces completePieces
                    askSenderQueue $ SenderQueueMessage $ TM.BitField bitfield
                    askSenderQueue $ SenderQueueMessage $ TM.Unchoke
                    modify $ \st -> st { _weChoke = False }

                Right message -> do
                    handleMessage message

        PeerHandlerFromSender transferred -> do
            updateUpRate transferred

        PeerHandlerFromChokeManager fromChokeM -> do
            handleChokeManagerMessage fromChokeM

        PeerHandlerTick -> do
            timerTick


handleMessage :: TM.Message -> Process PConf PState ()
handleMessage message = do
    case message of
        TM.KeepAlive
            -> return ()
        TM.Choke
            -> handleChokeMessage

        TM.Unchoke
            -> do
                modify $ \st -> st { _peerChoke = False }
                fillBlocks

        TM.Interested
            -> modify $ \st -> st { _peerInterested = True  }

        TM.NotInterested
            -> modify $ \st -> st { _peerInterested = False }

        TM.Have pieceNum
            -> handleHaveMessage pieceNum

        TM.BitField bitfield
            -> handleBitFieldMessage bitfield

        TM.Request pieceNum block
            -> handleRequestMessage pieceNum block

        TM.Piece pieceNum offset bs
            -> handlePieceMessage pieceNum offset bs

        TM.Cancel pieceNum block
            -> handleCancelMessage pieceNum block

        TM.Port _
            -> return () -- no DHT yet, ignore


handleChokeMessage :: Process PConf PState ()
handleChokeMessage = do
    -- TODO clear sender queue
    putbackBlocks
    modify $ \st -> st { _peerChoke = True  }


handleHaveMessage :: PieceNum -> Process PConf PState ()
handleHaveMessage pieceNum = do
    pieceArray <- asks _pieceArray
    peerPieces <- gets _peerPieces

    let (lo, hi) = A.bounds pieceArray
    if pieceNum >= lo && pieceNum <= hi
        then do
            debugP $ "peer has piece #" ++ show pieceNum
            PS.have pieceNum peerPieces
            trackInterestAdd [pieceNum]
            decMissingCounter 1
            fillBlocks
        else do
            errorP $ "unknown piece #" ++ show pieceNum
            stopProcess


handleBitFieldMessage :: B.ByteString -> Process PConf PState ()
handleBitFieldMessage bitfield = do
    numPieces  <- asks _numPieces
    pieceSet   <- gets _peerPieces
    pieceArray <- asks _pieceArray
    piecesNull <- PS.null pieceSet
    if piecesNull
        then do
            peerPieces <- createPeerPieces numPieces bitfield
            pieceList  <- PS.toList peerPieces
            trackInterestAdd pieceList
            decMissingCounter (fromIntegral . length $ pieceList)
            modify $ \st -> st { _peerPieces = peerPieces }
        else do
            errorP "got out of band bitfield request, dying"
            stopProcess


handleRequestMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleRequestMessage pieceNum block = do
    choking <- gets _weChoke
    unless choking $ do
        debugP $ "Peer requested: " ++ show pieceNum ++ "(" ++ show block ++ ")"
        askSenderQueue $ SenderQueuePiece pieceNum block


handlePieceMessage :: PieceNum -> Integer -> B.ByteString -> Process PConf PState ()
handlePieceMessage pieceNum offset bs = do
    blockQueue <- gets _blockQueue

    let size  = fromIntegral $ B.length bs
        block = PieceBlock offset size
        record = (pieceNum, block)
    if S.member record blockQueue
        then do
            storeBlock pieceNum block bs
            modify $ \st -> st { _blockQueue = S.delete record blockQueue }
        else do
            return ()


handleCancelMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleCancelMessage pieceNum block = do
    askSenderQueue $ SenderQueueCancelPiece pieceNum block


handleChokeManagerMessage :: PeerChokeMessage -> Process PConf PState ()
handleChokeManagerMessage message = do
    case message of
        ChokePeer -> do
            weChoke <- gets _weChoke
            when (not weChoke) $ do
                 askSenderQueue $ SenderQueueMessage TM.Choke
                 modify $ \st -> st { _weChoke = True }
        UnchokePeer -> do
            weChoke <- gets _weChoke
            when weChoke $ do
                 askSenderQueue $ SenderQueueMessage TM.Unchoke
                 modify $ \st -> st { _weChoke = False }

        PieceComplete pieceNum -> do
            askSenderQueue $ SenderQueueMessage $ TM.Have pieceNum
            trackInterestRemove pieceNum


timerTick :: Process PConf PState ()
timerTick = do
    fromChan <- asks _fromChan
    upRate   <- gets _upRate
    downRate <- gets _downRate
    liftIO $ setTimeout 5 $ atomically $ writeTChan fromChan PeerHandlerTick
    (upRate', downRate')   <- timerTickStatus upRate downRate
    (upRate'', downRate'') <- timerTickChokeManager upRate' downRate'
    modify $ \st -> st { _upRate = upRate'', _downRate = downRate'' }
    fillBlocks


timerTickChokeManager :: R.Rate -> R.Rate -> Process PConf PState (R.Rate, R.Rate)
timerTickChokeManager upRate downRate =  do
   rateV      <- asks _rateV
   peerChoke  <- gets _peerChoke
   interested <- gets _peerInterested
   threadId   <- liftIO $ myThreadId
   timestamp  <- liftIO $ getCurrentTime

   let (peerUpRate, newUpRate)     = R.extractRate timestamp upRate
       (peerDownRate, newDownRate) = R.extractRate timestamp downRate
       peerRate = PeerRate
           { _peerRateUpRate = peerUpRate
           , _peerRateDownRate = peerDownRate
           , _peerRateInterested = interested
           , _peerRateChokingUs = peerChoke
           }
   liftIO . atomically $ do
       peerRateList <- readTVar rateV
       writeTVar rateV ((threadId, peerRate) : peerRateList)
   return (newUpRate, newDownRate)


timerTickStatus :: R.Rate -> R.Rate -> Process PConf PState (R.Rate, R.Rate)
timerTickStatus upRate downRate = do
    let (peerUpCount, newUpRate)     = R.extractCount $ upRate
        (peerDownCount, newDownRate) = R.extractCount $ downRate
    statV    <- asks _statV
    infoHash <- asks _infoHash
    let stat = UpDownStat
            { _statInfoHash   = infoHash
            , _statUploaded   = fromIntegral peerUpCount
            , _statDownloaded = fromIntegral peerDownCount
            }
    liftIO . atomically $ do
       statList <- readTVar statV
       writeTVar statV (stat : statList)
    return (newUpRate, newDownRate)


putbackBlocks :: Process PConf PState ()
putbackBlocks = do
    blockQueue <- gets _blockQueue
    askPieceManager $ PieceManagerPutbackBlock $ S.toList blockQueue
    modify $ \st -> st { _blockQueue = S.empty }


trackInterestAdd :: [PieceNum] -> Process PConf PState ()
trackInterestAdd pieceNumList = do
    haveV       <- asks _haveV
    interestSet <- gets _interestPieces

    askPieceManager $ PieceManagerPeerHave pieceNumList haveV
    interestSlice <- liftIO . atomically $ takeTMVar haveV


    let interestSet' = updateInterestSet interestSlice interestSet
    if S.null interestSet'
        then do
            modify $ \st -> st { _interestPieces = S.empty }
        else do
            modify $ \st -> st { _interestPieces = interestSet'
                               , _weInterested = True
                               }
            askSenderQueue $ SenderQueueMessage TM.Interested
  where
    updateInterestSet slice set = foldl (flip S.insert) set slice


trackInterestRemove :: PieceNum -> Process PConf PState ()
trackInterestRemove pieceNum = do
    interestSet <- gets _interestPieces
    let interestSet' = S.delete pieceNum interestSet
    when (S.null interestSet') $ do
        modify $ \st -> st { _weInterested = False }
        askSenderQueue $ SenderQueueMessage TM.NotInterested
    modify $ \st -> st { _interestPieces = interestSet' }


decMissingCounter :: Integer -> Process PConf PState ()
decMissingCounter n = do
    modify $ \st -> st { _missingPieces = (_missingPieces st) - n }


fillBlocks :: Process PConf PState ()
fillBlocks = do
    choked     <- gets _peerChoke
    interested <- gets _weInterested
    when (not choked && interested) checkWatermark


loMark :: Integer
loMark = 5

hiMark :: Integer
hiMark = 25

endgameLoMark :: Integer
endgameLoMark = 1

checkWatermark :: Process PConf PState ()
checkWatermark = do
    queue   <- gets _blockQueue
    endgame <- gets _isEndgame
    let size = fromIntegral $ S.size queue
        mark = if endgame then endgameLoMark else loMark
    when (size < mark) $ do
        toQueue <- grabBlocks (fromIntegral (hiMark - size))
        queuePieces toQueue


grabBlocks :: Int -> Process PConf PState [(PieceNum, PieceBlock)]
grabBlocks k = do
    blockV     <- asks _blockV
    peerPieces <- gets _peerPieces
    askPieceManager $ PieceManagerGrabBlock k peerPieces blockV
    response   <- liftIO . atomically $ takeTMVar blockV
    case response of
        (Leech, blocks) -> do
            return blocks
        (Endgame, blocks) -> do
            modify $ \st -> st { _isEndgame = True }
            return blocks


queuePieces :: [(PieceNum, PieceBlock)] -> Process PConf PState ()
queuePieces toQueue = do
    blockQueue <- gets _blockQueue
    toQueueFiltered <- forM toQueue $ \(piece, block) -> do
        if S.member (piece, block) blockQueue
            then do
                return Nothing
            else do
                askSenderQueue $ SenderQueueMessage $ TM.Request piece block
                return $ Just (piece, block)
    modify $ \st -> st { _blockQueue = S.union blockQueue (S.fromList $ catMaybes toQueueFiltered) }


storeBlock :: PieceNum -> PieceBlock -> B.ByteString -> Process PConf PState ()
storeBlock pieceNum block bs = do
    askPieceManager $ PieceManagerStoreBlock pieceNum block bs


askPieceManager :: PieceManagerMessage -> Process PConf PState ()
askPieceManager message = do
   pieceMChan <- asks _pieceMChan
   liftIO . atomically $ writeTChan pieceMChan message


askSenderQueue :: SenderQueueMessage -> Process PConf PState ()
askSenderQueue message = do
    sendChan <- asks _sendChan
    liftIO . atomically $ writeTChan sendChan message


updateUpRate :: Integer -> Process PConf PState ()
updateUpRate transferred = do
    upRate <- gets _upRate
    modify $ \st -> st { _upRate = R.updateBytes transferred upRate }


updateDownRate :: Integer -> Process PConf PState ()
updateDownRate transferred = do
    downRate <- gets _downRate
    modify $ \st -> st { _downRate = R.updateBytes transferred downRate }


createPeerPieces :: Integer -> B.ByteString -> Process PConf PState PS.PieceSet
createPeerPieces numPieces bitfield =
    PS.fromList numPieces (TM.readBitField bitfield)


