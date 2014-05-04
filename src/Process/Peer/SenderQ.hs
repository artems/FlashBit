module Process.Peer.SenderQ
  ( runPeerSenderQ
  , SenderQMessage(..)
  )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (foldl')

import Channels
import Process
import Process.FS hiding (start)
import qualified Data.Queue as Q
import Supervisor
import Torrent
import Protocol.Wire


data SenderQMessage
    = SenderQM Message
    | SenderOChoke
    | SenderQPiece PieceNum PieceBlock
    | SenderQCancel PieceNum PieceBlock
    | SenderQRequestPrune PieceNum PieceBlock -- ^ Prune SendQueue of this (pn, blk) pair


data PConf = PConf
    { _sqIn :: TChan SenderQMessage
    , _sqOut :: TMVar B.ByteString
    , _readBlockV :: TMVar B.ByteString
    , _fileAgentChan :: TChan FileAgentMessage
    , _peerHandlerChan :: TChan PeerHandlerMessage
    }

data PState = PState
    { _outQueue         :: Q.Queue (Either Message (PieceNum, PieceBlock))
    , _bytesTransferred :: Int
    }

instance ProcessName PConf where
    processName _ = "Process.Peer.SenderQueue"


runPeerSenderQ
    :: [Capabilities] -> TMVar B.ByteString -> TChan SenderQMessage
    -> TChan PeerHandlerMessage -> TChan FileAgentMessage -> IO ()
runPeerSenderQ caps abcV senderChan peerHChan fileAgentChan = do
    rbtv <- liftIO newEmptyTMVarIO
    let pconf  = PConf senderChan abcV rbtv fileAgentChan peerHChan
        pstate = Q.empty 0
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState StatusMessage
wait = do
    statusChan <- asks _statusChan
    liftIO . atomically $ readTChan statusChan


receive :: StatusMessage -> Process PConf PState ()
receive message =


pgm :: Process PConf PState ()
pgm = do
    queue <- gets _outQueue
    bytes <- gets _bytesTransferred
    when (bytes > 0) rateUpdateEvent
    ic <- asks _sqIn
    ov <- asks _sqOut
    result <- case Q.first queue of
        Nothing ->
            liftIO . atomically $ readTChan ic >>= return . Right
        Just r -> do
            p <- case r of
                    Left m -> return m
                    Right (pieceNum, block) -> do
                        bs <- readBlock pieceNum block
                        return $ Piece pieceNum (_pieceBlockOffset block) bs
            let bs = encodePacket p
                sz = fromIntegral $ B.length bs
            liftIO . atomically $
                (putTMVar ov bs >> return (Left sz)) `orElse`
                (readTChan ic >>= return . Right)
    case r of
        Left sz ->
            modify (\s -> s { bytesTransferred = bytesTransferred s + sz
                            , outQueue = Q.remove (outQueue s)})
        Right m ->
            case m of
                SenderQM msg -> modifyQ (Q.push $ Left msg)
                SenderQPiece n blk -> modifyQ (Q.push $ Right (n, blk))
                SenderQCancel n blk -> do
                    else modifyQ (Q.filter (filterPiece n blk))
                SenderOChoke -> do
                    modifyQ (Q.filter filterAllPiece)
                    modifyQ (Q.push $ Left Choke)
                SenderQRequestPrune n blk -> do
                    piece <- partitionQ (pickRequest n blk)
                    case piece of
                      []  -> modifyQ (Q.push (Left $ Cancel n blk)) -- Request must have been sent
                      [_] -> return ()
                      ps  -> fail $ "Impossible case, SenderQRequestPrune " ++ show ps
    pgm

rateUpdateEvent :: Process CF ST ()
rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
    l <- gets bytesTransferred
    bwc <- asks peerCtlCh
    liftIO . atomically $ writeTChan bwc (FromSenderQ l)
    modify (\s -> s { bytesTransferred = 0 })

-- The type of the Outgoing queue
type OutQT = Either Message (PieceNum, Block)

filterAllPiece :: OutQT -> Bool
filterAllPiece (Right _) = True
filterAllPiece (Left  _) = False

pickPiece :: PieceNum -> Block -> OutQT -> Bool
pickPiece n blk (Right (n1, blk1)) | n == n1 && blk == blk1 = True
pickPiece _ _   _                                           = False

filterPiece :: PieceNum -> Block -> OutQT -> Bool
filterPiece n blk (Right (n1, blk1)) | n == n1 && blk == blk1 = False
                                     | otherwise              = True
filterPiece _ _   _                                           = True

pickRequest :: PieceNum -> Block -> OutQT -> Bool
pickRequest n blk (Left (Request n1 blk1)) | n == n1 && blk == blk1 = True
pickRequest _ _   _                                                 = False

modifyQ :: (Q.Queue (OutQT) ->
            Q.Queue (OutQT))
                    -> Process CF ST ()
modifyQ f = modify (\s -> s { outQueue = f $! outQueue s })

partitionQ :: (OutQT -> Bool) -> Process CF ST [OutQT]
partitionQ p = do
    s <- get
    let (as, nq) = Q.partition p $ outQueue s
    put $! s { outQueue = nq }
    return as

-- | Read a block from the filesystem for sending
readBlock :: PieceNum -> Block -> Process CF ST B.ByteString
readBlock pn blk = do
    v <- asks readBlockTV
    fch <- asks fsCh
    liftIO $ do
        atomically $ writeTChan fch (ReadBlock pn blk v)
        atomically $ takeTMVar v

