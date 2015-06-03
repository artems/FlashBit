module FlashBit.PieceManager
    ( runPieceManager
    ) where

import Control.Concurrent.STM
import Control.Monad.Reader (when, liftIO, asks)
import qualified Data.PieceSet as PS
import qualified Data.ByteString as B

import Process
import Torrent
import FlashBit.PieceManager.Chan
import FlashBit.PieceManager.State
import qualified FlashBit.FileAgent as FileAgent
import qualified FlashBit.TorrentDatabase as TorrentDatabase


data PConf = PConf
    { _infoHash         :: InfoHash
    , _checkTMV         :: TMVar Bool
    , _torrentTV        :: TorrentDatabase.TorrentTVar
    , _fileAgentChan    :: TChan FileAgent.FileAgentMessage
    , _broadcastChan    :: TChan PieceBroadcastMessage
    , _pieceManagerChan :: TChan PieceManagerMessage
    }

instance ProcessName PConf where
    processName pconf = "PieceManager [" ++ showInfoHash (_infoHash pconf) ++ "]"

type PState = PieceManagerState


runPieceManager
    :: InfoHash -> PieceArray -> PieceHaveMap
    -> TorrentDatabase.TorrentTVar
    -> TChan FileAgent.FileAgentMessage
    -> TChan PieceBroadcastMessage
    -> TChan PieceManagerMessage
    -> IO ()
runPieceManager infoHash pieceArray pieceHaveMap torrentTV fileAgentChan broadcastChan pieceManagerChan = do
    checkTMV       <- newEmptyTMVarIO
    let pconf  = PConf infoHash checkTMV torrentTV fileAgentChan broadcastChan pieceManagerChan
        pstate = mkPieceManagerState pieceHaveMap pieceArray
    wrapProcess pconf pstate process

process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process

wait :: Process PConf PState PieceManagerMessage
wait = do
    pieceChan <- asks _pieceManagerChan
    liftIO . atomically $ readTChan pieceChan

receive :: PieceManagerMessage -> Process PConf PState ()
receive message = do
    case message of
        GrabBlock num peerPieces blockV -> do
            let pieces = PS.toSet peerPieces
            blocks <- grabBlocks num pieces
            liftIO . atomically $ putTMVar blockV blocks

        GetCompleted doneV -> do
            pieces <- getDonePieces
            liftIO . atomically $ putTMVar doneV pieces

        PeerHave pieces interestV -> do
            interested <- markPeerHave pieces
            liftIO . atomically $ putTMVar interestV interested

        StoreBlock pieceNum block pieceData -> do
            torrentTV <- asks _torrentTV
            -- TODO block complete endgame broadcast
            askWriteBlock pieceNum block pieceData
            pieceComplete <- storeBlock pieceNum block

            when pieceComplete $ do
                pieceOk <- askCheckPiece pieceNum
                if pieceOk
                    then do
                        pieceSize <- pieceLength pieceNum
                        liftIO . atomically $ 
                            TorrentDatabase.pieceCompletedSTM torrentTV pieceSize
                        broadcastPieceComplete pieceNum
                        torrentComplete <- markPieceDone pieceNum
                        when torrentComplete $ do
                            debugP $ "Полностью скачан торрент"
                    else do
                        putbackPiece pieceNum

        PutbackBlock blocks -> do
            mapM_ putbackBlock blocks


askWriteBlock :: PieceNum -> PieceBlock -> B.ByteString -> Process PConf PState ()
askWriteBlock pieceNum block pieceData = do
    fileAgentChan <- asks _fileAgentChan
    let message = FileAgent.WriteBlock pieceNum block pieceData
    liftIO . atomically $ writeTChan fileAgentChan message

askCheckPiece :: PieceNum -> Process PConf PState Bool
askCheckPiece pieceNum = do
    checkTMV      <- asks _checkTMV
    fileAgentChan <- asks _fileAgentChan
    let message = FileAgent.CheckPiece pieceNum checkTMV
    liftIO . atomically $ writeTChan fileAgentChan message
    liftIO . atomically $ takeTMVar checkTMV

broadcastPieceComplete :: PieceNum -> Process PConf PState ()
broadcastPieceComplete pieceNum = do
    broadcastChan <- asks _broadcastChan
    liftIO . atomically $ writeTChan broadcastChan $ PieceComplete pieceNum
