module Process.FileAgent
    ( FileAgentMessage(..)
    , runFileAgent
    ) where


import Data.Array ((!))
import Data.ByteString as B

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

import FS
import Torrent
import Process
import Process.Channel


data FileAgentMessage
    = FileAgentReadBlock PieceNum PieceBlock (TMVar B.ByteString)
    | FileAgentWriteBlock PieceNum PieceBlock B.ByteString
    | FileAgentCheckPiece PieceNum (TMVar Bool)


data PConf = PConf
    { _target        :: TorrentFile
    , _pieceArray    :: PieceArray
    , _fileAgentChan :: TChan FileAgentMessage
    }

instance ProcessName PConf where
    processName _ = "FileAgent"


type PState = ()


runFileAgent :: TorrentFile -> PieceArray -> TChan FileAgentMessage -> IO ()
runFileAgent target pieceArray fileAgentChan = do
    let pconf = PConf target pieceArray fileAgentChan
    wrapProcess pconf () process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState FileAgentMessage
wait = do
    fileAgentChan <- asks _fileAgentChan
    liftIO . atomically $ readTChan fileAgentChan


receive :: FileAgentMessage -> Process PConf PState ()
receive message = do
    target     <- asks _target
    pieceArray <- asks _pieceArray

    case message of
        FileAgentReadBlock pieceNum block pieceV -> do
            debugP $ "reading block #" ++ show pieceNum ++ " " ++
                   "(" ++ show (_blockOffset block) ++ ", " ++ show (_blockSize block) ++ ")"
            pieceData <- liftIO $ readBlock target pieceNum block pieceArray
            liftIO . atomically $ putTMVar pieceV pieceData

        FileAgentWriteBlock pieceNum block pieceData -> do
            debugP $ "writing block #" ++ show pieceNum ++ " " ++
                   "(" ++ show (_blockOffset block) ++ ", " ++ show (_blockSize block) ++ ")"
            liftIO $ writeBlock target pieceNum block pieceArray pieceData

        FileAgentCheckPiece pieceNum checkV -> do
            let piece = pieceArray ! pieceNum
            checkResult <- liftIO $ FS.checkPiece target piece
            liftIO . atomically $ putTMVar checkV checkResult


