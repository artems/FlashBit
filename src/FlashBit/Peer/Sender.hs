module FlashBit.Peer.Sender
    ( SenderMessage(..)
    , runPeerSender
    ) where

import Control.Concurrent.STM
import Control.Concurrent.Timer
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (liftIO, asks)
import qualified Data.ByteString as B
import qualified Network.Socket as S (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Torrent
import qualified Torrent.Message as TM
import FlashBit.Peer.Sender.State
import qualified FlashBit.FileAgent as FileAgent


data SenderMessage
    = SenderPiece PieceNum PieceBlock
    | SenderMessage TM.Message
    | SenderKeepAlive
    | SenderCancelPiece PieceNum PieceBlock

data PConf = PConf
    { _prefix        :: String
    , _socket        :: S.Socket
    , _sendTV        :: TVar Integer
    , _pieceV        :: TMVar B.ByteString
    , _fileAgentChan :: TChan FileAgent.FileAgentMessage
    , _sendChan      :: TChan SenderMessage
    }

instance ProcessName PConf where
    processName pconf = "Peer.Sender [" ++ _prefix pconf ++ "]"

type PState = PeerSenderState


keepAliveInterval :: Int
keepAliveInterval = 120


runPeerSender :: String -> S.Socket -> TVar Integer
              -> TChan FileAgent.FileAgentMessage
              -> TChan SenderMessage
              -> IO ()
runPeerSender prefix socket sendTV fileAgentChan sendChan = do
    pieceV     <- newEmptyTMVarIO
    timerId    <- setKeepAliveTimer sendChan
    let pconf  = PConf prefix socket sendTV pieceV fileAgentChan sendChan
        pstate = mkPeerSenderState timerId
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState (Maybe SenderMessage)
wait = do
    sendChan  <- asks _sendChan
    liftIO . atomically $ tryReadTChan sendChan


receive :: Maybe SenderMessage -> Process PConf PState ()
receive Nothing = do
    sendChan  <- asks _sendChan
    mbMessage <- firstQ
    case mbMessage of
        Just message -> do
            encodePacket message >>= sendMessage
            updateKeepAliveTimer
        Nothing -> do
            -- очередь пуста, блокируем процесс до первого сообщения
            liftIO . atomically $ peekTChan sendChan >> return ()

receive (Just command) = do
    case command of
        SenderPiece pieceNum block -> do
            pushQ $ Right (pieceNum, block)

        SenderMessage message -> do
            case message of
                TM.Choke ->
                    pruneAllPieceRequests
                TM.Cancel pieceNum block ->
                    prunePieceRequest pieceNum block
                _ -> return ()
            pushQ $ Left message

        SenderKeepAlive ->
            pushQ $ Left TM.KeepAlive

        SenderCancelPiece pieceNum block ->
            prunePieceMessage pieceNum block


encodePacket :: QueueType -> Process PConf PState TM.Message
encodePacket (Left message) = do
    return message
encodePacket (Right (pieceNum, block)) = do
    pieceData <- getPieceBlock pieceNum block
    return $ TM.Piece pieceNum (_blockOffset block) pieceData


getPieceBlock :: PieceNum -> PieceBlock -> Process PConf PState B.ByteString
getPieceBlock pieceNum block = do
    pieceV        <- asks _pieceV
    fileAgentChan <- asks _fileAgentChan
    let message = FileAgent.ReadBlock pieceNum block pieceV
    liftIO . atomically $ writeTChan fileAgentChan message
    liftIO . atomically $ takeTMVar pieceV


sendPacket :: B.ByteString -> Process PConf PState ()
sendPacket packet = do
    socket <- asks _socket
    sendTV <- asks _sendTV
    liftIO $ SB.sendAll socket packet
    liftIO . atomically $ do
        let size = fromIntegral $ B.length packet
        transferred <- readTVar sendTV
        writeTVar sendTV $ transferred + size


sendMessage :: TM.Message -> Process PConf PState ()
sendMessage message = sendPacket $ TM.encodeMessage message


updateKeepAliveTimer :: Process PConf PState ()
updateKeepAliveTimer = do
    sendChan   <- asks _sendChan
    oldTimerId <- gets _keepAliveTimer
    liftIO $ clearTimeout oldTimerId
    newTimerId <- liftIO $ setKeepAliveTimer sendChan
    modify $ \st -> st { _keepAliveTimer = newTimerId }


setKeepAliveTimer :: TChan SenderMessage -> IO TimerId
setKeepAliveTimer sendChan =
    setTimeout keepAliveInterval $
        atomically $ writeTChan sendChan SenderKeepAlive
