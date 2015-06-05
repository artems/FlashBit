module FlashBit.Peer.Main
    ( runPeerMain
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time

import Process
import FlashBit.Peer.Common
import FlashBit.Peer.Sender
import FlashBit.PeerDatabase hiding (_peerChan)
import qualified FlashBit.TorrentDatabase as TorrentDatabase
import FlashBit.PieceManager.Chan as PieceManager

import Torrent
import qualified Torrent.Message as TM


data PConf = PConf
    { _prefix             :: String
    , _torrent            :: Torrent
    , _sendTV             :: TVar Integer
    , _receiveTV          :: TVar Integer
    , _haveV              :: TMVar [PieceNum]
    , _blockV             :: TMVar (PieceMode, [(PieceNum, PieceBlock)])
    , _peerTV             :: PeerTVar
    , _sendChan           :: TChan SenderMessage
    , _peerChan           :: TChan PeerMessage
    , _torrentTV          :: TorrentDatabase.TorrentTVar
    , _pieceManagerChan   :: TChan PieceManager.PieceManagerMessage
    , _pieceBroadcastChan :: TChan PieceManager.PieceBroadcastMessage
    }

instance ProcessName PConf where
    processName pconf = "Peer [" ++ _prefix pconf ++ "]"

type PState = ()


tickInterval :: Int
tickInterval = 5

runPeerMain :: String -> TVar Integer -> TVar Integer -> PeerTVar
            -> TorrentDatabase.TorrentTVar
            -> TChan SenderMessage
            -> TChan PeerMessage
            -> IO ()
runPeerMain prefix sendTV receiveTV peerTV torrentTV sendChan peerChan = do
    torrent'               <- atomically $ readTVar torrentTV
    let torrent            = TorrentDatabase._torrent torrent'
    let channel            = TorrentDatabase._torrentChannel torrent'
    let pieceManagerChan   = TorrentDatabase._torrentPieceManagerChan channel
    let pieceBroadcastChan = TorrentDatabase._torrentPieceBroadcastChan channel
    pieceBroadcastChanDup  <- atomically $ dupTChan pieceBroadcastChan

    haveV  <- newEmptyTMVarIO
    blockV <- newEmptyTMVarIO
    let pconf = PConf
            { _prefix             = prefix
            , _torrent            = torrent
            , _haveV              = haveV
            , _blockV             = blockV
            , _sendTV             = sendTV
            , _receiveTV          = receiveTV
            , _peerTV             = peerTV
            , _torrentTV          = torrentTV
            , _sendChan           = sendChan
            , _peerChan           = peerChan
            , _pieceManagerChan   = pieceManagerChan
            , _pieceBroadcastChan = pieceBroadcastChanDup
            }
    let pstate = ()
    _timerId <- setTimeout tickInterval . atomically $ writeTChan peerChan Tick
    catchProcess pconf pstate (startup >> process) terminate

terminate :: PConf -> IO ()
terminate pconf = atomically $ do
    peerPieces <- getPeerPiecesSTM peerTV
    blockQueue <- getPeerBlockQueue peerTV
    let pieces = S.toList peerPieces
    writeTChan pieceManagerChan $ PieceManager.PeerUnhave pieces
    writeTChan pieceManagerChan $ PieceManager.PutbackBlock blockQueue
  where
    peerTV = _peerTV pconf
    pieceManagerChan = _pieceManagerChan pconf

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState (Either PeerMessage PieceBroadcastMessage)
wait = do
    peerChan           <- asks _peerChan
    pieceBroadcastChan <- asks _pieceBroadcastChan
    liftIO . atomically $
        (readTChan peerChan >>= return . Left) `orElse`
        (readTChan pieceBroadcastChan >>= return . Right)

receive :: Either PeerMessage PieceBroadcastMessage -> Process PConf PState ()
receive (Left message) = do
    case message of
        FromPeer fromPeer -> do
            handleMessage fromPeer

        FromChokeManager isChoke -> do
            peerTV    <- asks _peerTV
            weChoking <- liftIO . atomically $ isWeChokingSTM peerTV
            handleChokeManagerMessage isChoke weChoking

        Tick -> tick

receive (Right message) = do
    peerTV <- asks _peerTV

    case message of
        PieceManager.BlockComplete _pieceNum _block ->
            -- TODO: в режиме endgame, нужно отменить запрос такого же блока, если он был.
            return ()

        PieceManager.PieceComplete pieceNum -> do
            tellSender $ SenderMessage (TM.Have pieceNum)
            weNotInterestedNow <- liftIO . atomically $
                trackNotInterestedStateSTM [pieceNum] peerTV
            when weNotInterestedNow $
                tellSender $ SenderMessage TM.NotInterested

        PieceManager.TorrentComplete ->
            -- TODO: переключить состояние в сидера
            return ()

startup :: Process PConf PState ()
startup = do
    bitfield <- buildBitField
    tellSender $ SenderMessage (TM.BitField bitfield)

buildBitField :: Process PConf PState B.ByteString
buildBitField = do
    haveV   <- asks _haveV
    torrent <- asks _torrent
    let pieceArray = Torrent._torrentPieceArray torrent
    let numPieces  = pieceArraySize pieceArray
    tellPieceManager $ PieceManager.GetCompleted haveV
    completedPieces <- liftIO . atomically $ takeTMVar haveV
    return $ TM.encodeBitField numPieces completedPieces

handleMessage :: TM.Message -> Process PConf PState ()
handleMessage message = do
    case message of
        TM.KeepAlive ->
            return ()

        TM.Choke ->
            handleChokeMessage

        TM.Unchoke ->
            handleUnchokeMessage

        TM.Interested -> do
            peerTV <- asks _peerTV
            liftIO . atomically $ receiveInterestedSTM peerTV

        TM.NotInterested -> do
            peerTV <- asks _peerTV
            liftIO . atomically $ receiveNotInterestedSTM peerTV

        TM.Have pieceNum ->
            handleHaveMessage pieceNum

        TM.BitField bitfield ->
            handleBitFieldMessage bitfield

        TM.Request pieceNum block ->
            handleRequestMessage pieceNum block

        TM.Piece pieceNum offset bs ->
            handlePieceMessage pieceNum offset bs

        TM.Cancel pieceNum block ->
            handleCancelMessage pieceNum block

        TM.Port _ ->
            return ()

handleChokeMessage :: Process PConf PState ()
handleChokeMessage = do
    peerTV     <- asks _peerTV
    blockQueue <- liftIO . atomically $ receiveChokeSTM peerTV
    tellPieceManager $ PieceManager.PutbackBlock blockQueue

handleUnchokeMessage :: Process PConf PState ()
handleUnchokeMessage = do
    peerTV <- asks _peerTV
    liftIO . atomically $ receiveUnchokeSTM peerTV
    fillupBlockQueue

handleHave :: [PieceNum] -> Process PConf PState ()
handleHave [] = return ()
handleHave pieceNum = do
    haveV  <- asks _haveV
    peerTV <- asks _peerTV
    tellPieceManager $ PieceManager.PeerHave pieceNum haveV
    interested <- liftIO . atomically $ takeTMVar haveV
    weInterestedNow <- liftIO . atomically $
        trackInterestedStateSTM interested peerTV
    when weInterestedNow $
        tellSender $ SenderMessage TM.Interested
    fillupBlockQueue

validatePieceNum :: [PieceNum] -> Process PConf PState ()
validatePieceNum []              = return ()
validatePieceNum (pieceNum : ps) = do
    torrent <- asks _torrent
    unless (checkPieceNum torrent pieceNum) $ do
        errorP $ "unknown piece #" ++ show pieceNum
        stopProcess
    validatePieceNum ps

handleHaveMessage :: PieceNum -> Process PConf PState ()
handleHaveMessage pieceNum = do
    peerTV <- asks _peerTV
    liftIO . atomically $ receiveHaveSTM pieceNum peerTV
    validatePieceNum [pieceNum] >> handleHave [pieceNum]

handleBitFieldMessage :: B.ByteString -> Process PConf PState ()
handleBitFieldMessage bitfield = do
    peerTV <- asks _peerTV
    pieceSetIsEmpty <- liftIO . atomically $ isPieceSetEmptySTM peerTV
    when (not pieceSetIsEmpty) $ do
        errorP "got out of band bitfield request"
        stopProcess
    pieceNum <- liftIO . atomically $ receiveBitfieldSTM bitfield peerTV
    validatePieceNum pieceNum >> handleHave pieceNum

handleRequestMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleRequestMessage pieceNum block = do
    peerTV  <- asks _peerTV
    choking <- liftIO . atomically $ isWeChokingSTM peerTV
    when (not choking) $ tellSender (SenderPiece pieceNum block)

handlePieceMessage :: PieceNum -> Integer -> B.ByteString
                   -> Process PConf PState ()
handlePieceMessage pieceNum offset bs = do
    peerTV   <- asks _peerTV
    let size = fromIntegral $ B.length bs
    storeNeeded <- liftIO . atomically $
        receivePieceSTM pieceNum offset bs peerTV
    when storeNeeded $ storeBlock (PieceBlock offset size)
    fillupBlockQueue
  where
    storeBlock block = tellPieceManager $
        PieceManager.StoreBlock pieceNum block bs

handleCancelMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleCancelMessage pieceNum block =
    tellSender $ SenderCancelPiece pieceNum block

handleChokeManagerMessage :: Bool -> Bool -> Process PConf PState ()
handleChokeManagerMessage isChoke weChoking
    | isChoke && not weChoking = do
        peerTV <- asks _peerTV
        tellSender $ SenderMessage TM.Choke
        liftIO . atomically $ setChokeSTM peerTV
    | not isChoke && weChoking = do
        peerTV <- asks _peerTV
        tellSender $ SenderMessage TM.Unchoke
        liftIO . atomically $ setUnchokeSTM peerTV
    | otherwise = return ()

tick :: Process PConf PState ()
tick = do
    peerTV      <- asks _peerTV
    sendTV      <- asks _sendTV
    receiveTV   <- asks _receiveTV
    torrentTV   <- asks _torrentTV
    currentTime <- liftIO Time.getCurrentTime
    liftIO . atomically $ do
        sended   <- readTVar sendTV
        writeTVar sendTV 0
        received <- readTVar receiveTV
        writeTVar receiveTV 0
        updateRateSTM peerTV currentTime sended received
        TorrentDatabase.transferredUpdateSTM torrentTV sended received
    scheduleNextTick

scheduleNextTick :: Process PConf PState ()
scheduleNextTick = do
    peerChan <- asks _peerChan
    _timerId <- liftIO . setTimeout tickInterval $
        atomically $ writeTChan peerChan Tick
    return ()

fillupBlockQueue :: Process PConf PState ()
fillupBlockQueue = do
    peerTV     <- asks _peerTV
    numToQueue <- liftIO . atomically $ numToQueueSTM peerTV
    when (numToQueue > 0) $ do
        toQueue0 <- askBlocks numToQueue
        toQueue1 <- liftIO . atomically $ queuePiecesSTM toQueue0 peerTV
        forM_ toQueue1 $ \(piece, block) -> do
            tellSender $ SenderMessage (TM.Request piece block)

askBlocks :: Integer -> Process PConf PState [(PieceNum, PieceBlock)]
askBlocks num = do
    peerTV <- asks _peerTV
    blockV <- asks _blockV
    pieces <- liftIO . atomically $ getPeerPiecesSTM peerTV
    tellPieceManager $ PieceManager.GrabBlock num pieces blockV
    response <- liftIO . atomically $ takeTMVar blockV
    case response of
        (Leech, blocks)   -> return blocks
        (Endgame, blocks) -> do
            liftIO . atomically $ setEndgameSTM peerTV
            return blocks

tellSender :: SenderMessage -> Process PConf PState ()
tellSender message = do
    sendChan <- asks _sendChan
    liftIO . atomically $ writeTChan sendChan message

tellPieceManager :: PieceManager.PieceManagerMessage -> Process PConf PState ()
tellPieceManager message = do
    pieceManagerChan <- asks _pieceManagerChan
    liftIO . atomically $ writeTChan pieceManagerChan message
