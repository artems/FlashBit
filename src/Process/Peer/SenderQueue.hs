module Process.Peer.SenderQueue
    ( SenderQueueMessage(..)
    , runPeerSenderQueue
    ) where


import qualified Data.Sequence as S
import qualified Data.ByteString as B

import Control.Concurrent.STM
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (liftIO, asks)

import Timer
import Process
import Process.Channel
import Process.FileAgent

import Torrent
import qualified Torrent.Message as TM


data SenderQueueMessage
    = SenderQueuePiece PieceNum PieceBlock
    | SenderQueueMessage TM.Message
    | SenderQueueKeepAlive
    | SenderQueueCancelPiece PieceNum PieceBlock

data PConf = PConf
    { _dropbox       :: TMVar TM.Message
    , _pieceDataV    :: TMVar B.ByteString
    , _senderChan    :: TChan SenderQueueMessage
    , _fileAgentChan :: TChan FileAgentMessage
    }

instance ProcessName PConf where
    processName _ = "Peer.SenderQueue"


data PState = PState
    { _queue            :: S.Seq QueueType
    , _transferred      :: Int
    , _lastMessageTimer :: TimerId
    }

type QueueType = Either TM.Message (PieceNum, PieceBlock)


runPeerSenderQueue :: TMVar TM.Message
                   -> TChan SenderQueueMessage
                   -> TChan FileAgentMessage
                   -> IO ()
runPeerSenderQueue dropbox senderChan fileAgentChan = do
    timerId    <- setTimeout 120 $ atomically $ writeTChan senderChan SenderQueueKeepAlive
    pieceDataV <- newEmptyTMVarIO
    let pconf  = PConf dropbox pieceDataV senderChan fileAgentChan
        pstate = PState S.empty 0 timerId
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState (Either () SenderQueueMessage)
wait = do
    senderChan <- asks _senderChan
    mbMessage  <- liftIO . atomically $ tryReadTChan senderChan
    case mbMessage of
        Nothing      -> return $ Left ()
        Just message -> return $ Right message


receive :: Either () SenderQueueMessage -> Process PConf PState ()
receive (Left _) = do
    dropbox    <- asks _dropbox
    senderChan <- asks _senderChan

    message <- firstQ
    case message of
        Just message -> do
            message' <- encodePacket message
            liftIO . atomically $ putTMVar dropbox message'
            updateKeepAliveTimer

        Nothing -> do
            -- очередь пуста, блокируем процесс до первого сообщения
            liftIO . atomically $ peekTChan senderChan >> return ()


receive (Right message) = do
    case message of
        SenderQueuePiece pieceNum block -> do
            pushQ $ Right (pieceNum, block)

        SenderQueueMessage message -> do
            case message of
                TM.Choke ->
                    -- filter all piece requests
                    modifyQ $ S.filter isNotPieceRequest

                TM.Cancel pieceNum block ->
                    -- filter the request if it is not sent
                    modifyQ $ S.filter (/= Left (TM.Request pieceNum block))
                _ -> return ()
            pushQ $ Left message

        SenderQueueKeepAlive ->
            pushQ $ Left TM.KeepAlive

        SenderQueueCancelPiece pieceNum block -> do
            modifyQ $ S.filter (== Right (pieceNum, block))
  where
    isNotPieceRequest (Left _)  = True
    isNotPieceRequest (Right _) = False



encodePacket :: QueueType -> Process PConf PState TM.Message
encodePacket (Left message) = do
    return message
encodePacket (Right (pieceNum, block)) = do
    pieceData <- readBlock pieceNum block
    return $ TM.Piece pieceNum (_blockOffset block) pieceData


readBlock :: PieceNum -> PieceBlock -> Process PConf PState B.ByteString
readBlock pieceNum block = do
    pieceDataV    <- asks _pieceDataV
    fileAgentChan <- asks _fileAgentChan
    liftIO $ do
        atomically $ writeTChan fileAgentChan $
            FileAgentReadBlock pieceNum block pieceDataV
        atomically $ takeTMVar pieceDataV


updateKeepAliveTimer :: Process PConf PState ()
updateKeepAliveTimer = do
    timerId    <- gets _lastMessageTimer
    senderChan <- asks _senderChan

    liftIO $ clearTimeout timerId
    timerId' <- liftIO $ setTimeout 120 $
        atomically $ writeTChan senderChan SenderQueueKeepAlive
    modify $ \st -> st { _lastMessageTimer = timerId' }


pushQ :: QueueType -> Process PConf PState ()
pushQ a = do
    modify $ \st -> st { _queue =  a S.<| (_queue st) }


firstQ :: Process PConf PState (Maybe QueueType)
firstQ = do
    queue <- gets _queue

    case S.viewr queue of
        S.EmptyR ->
            return Nothing
        queue' S.:> message -> do
            modify $ \st -> st { _queue = queue' }
            return $ Just message


modifyQ :: (S.Seq QueueType -> S.Seq QueueType) -> Process PConf PState ()
modifyQ func = do
    queue <- gets _queue
    modify $ \st -> st { _queue = func queue }


