module FlashBit.Peer.Main
    ( runPeerMain
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time

import Process
import FlashBit.Peer.Common
import FlashBit.Peer.Sender
import FlashBit.PeerDatabase hiding (_infoHash, _peerChan)
import FlashBit.TorrentDatabase as TorrentDatabase
import FlashBit.PieceManager.Chan as PieceManager

import Torrent
import qualified Torrent.Message as TM


data PConf = PConf
    { _prefix             :: String
    , _infoHash           :: InfoHash
    , _pieceArray         :: PieceArray
    , _sendTV             :: TVar Integer
    , _receiveTV          :: TVar Integer
    , _haveV              :: TMVar [PieceNum]
    , _blockV             :: TMVar (TorrentPieceMode, [(PieceNum, PieceBlock)])
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


runPeerMain :: String -> InfoHash -> PieceArray
            -> TVar Integer -> TVar Integer
            -> PeerTVar
            -> TChan SenderMessage
            -> TChan PeerMessage
            -> TorrentDatabase.TorrentTVar
            -> TChan PieceManager.PieceManagerMessage
            -> TChan PieceManager.PieceBroadcastMessage
            -> IO ()
runPeerMain prefix infoHash pieceArray sendTV receiveTV
    peerTV
    sendChan
    peerChan
    torrentTV
    pieceManagerChan
    pieceBroadcastChan = do
        pconf <- mkConf prefix infoHash pieceArray sendTV receiveTV
            peerTV
            sendChan
            peerChan
            torrentTV
            pieceManagerChan
            pieceBroadcastChan
        let pstate = ()
        _timerId <- setTimeout 5 . atomically $ writeTChan peerChan Tick
        wrapProcess pconf pstate (startup >> process)


mkConf :: String -> InfoHash -> PieceArray
       -> TVar Integer
       -> TVar Integer
       -> PeerTVar
       -> TChan SenderMessage
       -> TChan PeerMessage
       -> TorrentDatabase.TorrentTVar
       -> TChan PieceManager.PieceManagerMessage
       -> TChan PieceManager.PieceBroadcastMessage
       -> IO PConf
mkConf prefix infoHash pieceArray sendTV receiveTV
    peerTV
    sendChan
    peerChan
    torrentTV
    pieceManagerChan
    pieceBroadcastChan = do
        haveV  <- newEmptyTMVarIO
        blockV <- newEmptyTMVarIO
        return $ PConf
            { _prefix             = prefix
            , _infoHash           = infoHash
            , _pieceArray         = pieceArray
            , _sendTV             = sendTV
            , _receiveTV          = receiveTV
            , _haveV              = haveV
            , _blockV             = blockV
            , _peerTV             = peerTV
            , _sendChan           = sendChan
            , _peerChan           = peerChan
            , _torrentTV          = torrentTV
            , _pieceManagerChan   = pieceManagerChan
            , _pieceBroadcastChan = pieceBroadcastChan
            }


startup :: Process PConf PState ()
startup = do
    bitfield <- buildBitField
    askSender (SenderMessage (TM.BitField bitfield))

buildBitField :: Process PConf PState B.ByteString
buildBitField = do
    tv        <- asks _peerTV
    haveV     <- asks _haveV
    numPieces <- liftIO . atomically $ getNumPiecesSTM tv
    askPieceManager $ PieceManager.GetCompleted haveV
    completePieces <- liftIO . atomically $ takeTMVar haveV
    return $ TM.encodeBitField numPieces completePieces

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

receive :: Either PeerMessage PieceBroadcastMessage
        -> Process PConf PState ()
receive (Left message) = do
    case message of
        FromPeer fromPeer -> do
            handleMessage fromPeer

        FromChokeManager isChoke -> do
            peerTV <- asks _peerTV
            weChoking <- liftIO . atomically $ isWeChokingSTM peerTV
            handleChokeManagerMessage isChoke weChoking

        Tick -> do
            timerTick

receive (Right message) = do
    tv <- asks _peerTV

    case message of
        PieceManager.PieceComplete pieceNum -> do
            askSender $ SenderMessage (TM.Have pieceNum)
            weNotInterestedNow <- liftIO . atomically $
                trackNotInterestedStateSTM [pieceNum] tv
            when weNotInterestedNow $ askSender (SenderMessage TM.NotInterested)

        PieceManager.BlockComplete _pieceNum _block ->
            return ()

        PieceManager.TorrentComplete ->
            return ()


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
            tv <- asks _peerTV
            liftIO . atomically $ receiveInterestedSTM tv

        TM.NotInterested -> do
            tv <- asks _peerTV
            liftIO . atomically $ receiveNotInterestedSTM tv

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
            return () -- no DHT yet, ignore


handleChokeMessage :: Process PConf PState ()
handleChokeMessage = do
    tv <- asks _peerTV
    blockQueue <- liftIO . atomically $ receiveChokeSTM tv
    askPieceManager $ PieceManager.PutbackBlock blockQueue

handleUnchokeMessage :: Process PConf PState ()
handleUnchokeMessage = do
    tv <- asks _peerTV
    liftIO . atomically $ receiveUnchokeSTM tv
    fillupBlockQueue

handleHaveMessage :: PieceNum -> Process PConf PState ()
handleHaveMessage pieceNum = do
    tv <- asks _peerTV
    -- debugP $ "Пир сообщил, что имеет часть #" ++ show pieceNum
    liftIO . atomically $ receiveHaveSTM pieceNum tv
    checkPieceNumM [pieceNum]
    handleHaveMessage' [pieceNum]

checkPieceNumM :: [PieceNum] -> Process PConf PState ()
checkPieceNumM []              = return ()
checkPieceNumM (pieceNum : ps) = do
    pieceArray <- asks _pieceArray
    unless (checkPieceNum pieceArray pieceNum) $ do
        errorP $ "Unknown piece #" ++ show pieceNum
        stopProcess
    checkPieceNumM ps


handleHaveMessage' :: [PieceNum] -> Process PConf PState ()
handleHaveMessage' [] = return ()
handleHaveMessage' pieceNum = do
    tv <- asks _peerTV
    haveV <- asks _haveV
    askPieceManager $ PieceManager.PeerHave pieceNum haveV
    interested <- liftIO . atomically $ takeTMVar haveV
    when (not . null $ interested) $ do
        weInterestedNow <- liftIO . atomically $
            trackInterestedStateSTM interested tv
        when weInterestedNow $ askSender $ SenderMessage TM.Interested
    fillupBlockQueue

handleBitFieldMessage :: B.ByteString -> Process PConf PState ()
handleBitFieldMessage bitfield = do
    tv <- asks _peerTV
    pieceSetNull <- liftIO . atomically $ isPieceSetEmptySTM tv
    when (not pieceSetNull) $ do
        errorP "got out of band bitfield request, dying"
        stopProcess
    pieceNum <- liftIO . atomically $ receiveBitfieldSTM bitfield tv
    checkPieceNumM pieceNum
    handleHaveMessage' pieceNum

handleRequestMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleRequestMessage pieceNum block = do
    tv <- asks _peerTV
    choking <- liftIO . atomically $ isWeChokingSTM tv
    unless choking $ do
        debugP $ "Пир запросил часть #" ++ show pieceNum ++
                 " (" ++ show block ++ ")"
        askSender $ SenderPiece pieceNum block

handlePieceMessage :: PieceNum -> Integer -> B.ByteString
                   -> Process PConf PState ()
handlePieceMessage pieceNum offset bs = do
    tv <- asks _peerTV
    let size  = fromIntegral $ B.length bs
    storeNeeded <- liftIO . atomically $
        receivePieceSTM pieceNum offset bs tv
    when storeNeeded $ storeBlock (PieceBlock offset size)
    fillupBlockQueue
  where
    storeBlock block = askPieceManager $
        PieceManager.StoreBlock pieceNum block bs

handleCancelMessage :: PieceNum -> PieceBlock -> Process PConf PState ()
handleCancelMessage pieceNum block = do
    askSender $ SenderCancelPiece pieceNum block

handleChokeManagerMessage :: Bool -> Bool -> Process PConf PState ()
handleChokeManagerMessage isChoke weChoking
    | isChoke && not weChoking = do
        peerTV <- asks _peerTV
        askSender $ SenderMessage TM.Choke
        liftIO . atomically $ setChokeSTM peerTV
    | not isChoke && weChoking = do
        peerTV <- asks _peerTV
        askSender $ SenderMessage TM.Unchoke
        liftIO . atomically $ setUnchokeSTM peerTV
    | otherwise = return ()

timerTick :: Process PConf PState ()
timerTick = do
    tv          <- asks _peerTV
    sendTV      <- asks _sendTV
    receiveTV   <- asks _receiveTV
    currentTime <- liftIO Time.getCurrentTime
    (sended, received) <- liftIO . atomically $ do
        sended   <- readTVar sendTV
        writeTVar sendTV 0
        received <- readTVar receiveTV
        writeTVar receiveTV 0
        return (sended, received)
    liftIO . atomically $ do
        incUploadCounterSTM sended tv
        incDownloadCounterSTM received tv

    ((upload, _upRate), (download, _dnRate)) <- liftIO . atomically $
        extractRateSTM currentTime tv
    infoHash  <- asks _infoHash
    torrentTV <- asks _torrentTV
    let stat = UpDownStat infoHash upload download
    liftIO . atomically $ TorrentDatabase.transferredUpdateSTM torrentTV stat

    {-
    debugP $ "Пир имеет скорость" ++
        " приема: " ++ show upRate ++
        " отдачи: " ++ show dnRate ++
        " отдано байт: " ++ show upload ++
        " принято байт: " ++ show download
    -}

    peerChan <- asks _peerChan
    _timerId <- liftIO . setTimeout 5 $ atomically $ writeTChan peerChan Tick
    return ()

fillupBlockQueue :: Process PConf PState ()
fillupBlockQueue = do
    tv  <- asks _peerTV
    num <- liftIO . atomically $ numToQueueSTM tv
    when (num > 0) $ do
        toQueue <- grabBlocks num
        toQueueFiltered <- liftIO . atomically $ queuePiecesSTM toQueue tv
        forM_ toQueueFiltered $ \(piece, block) -> do
            askSender $ SenderMessage $ TM.Request piece block

grabBlocks :: Integer -> Process PConf PState [(PieceNum, PieceBlock)]
grabBlocks num = do
    tv         <- asks _peerTV
    blockV     <- asks _blockV
    peerPieces <- liftIO . atomically $ getPeerPiecesSTM tv
    askPieceManager $ PieceManager.GrabBlock num peerPieces blockV
    response   <- liftIO . atomically $ takeTMVar blockV
    case response of
        (Leech, blocks)   -> return blocks
        (Endgame, blocks) -> do
            liftIO . atomically $ setEndgameSTM tv
            return blocks

askSender :: SenderMessage -> Process PConf PState ()
askSender message = do
    sendChan <- asks _sendChan
    liftIO . atomically $ writeTChan sendChan message

askPieceManager :: PieceManager.PieceManagerMessage -> Process PConf PState ()
askPieceManager message = do
   pieceManagerChan <- asks _pieceManagerChan
   liftIO . atomically $ writeTChan pieceManagerChan message
