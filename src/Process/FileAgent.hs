module Process.FileAgent
    ( FileAgentMessage(..)
    , runFileAgent
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Data.Array ((!))
import Data.ByteString as B

import FS
import Torrent
import Process
import Process.Channel


data FileAgentMessage
    = ReadBlock PieceNum PieceBlock (TMVar B.ByteString)
    | WriteBlock PieceNum PieceBlock B.ByteString
    | CheckPiece PieceNum (TMVar Bool)


data PConf = PConf
    { _target     :: TorrentFile
    , _pieceArray :: PieceArray
    , _fsChan     :: TChan FileAgentMessage
    }

instance ProcessName PConf where
    processName _ = "FileAgent"

type PState = ()


runFileAgent :: TorrentFile -> PieceArray -> TChan FileAgentMessage -> IO ()
runFileAgent target pieceArray fsChan = do
    let pconf = PConf target pieceArray fsChan
    wrapProcess pconf () process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState FileAgentMessage
wait = do
    fsChan <- asks _fsChan
    liftIO . atomically $ readTChan fsChan


receive :: FileAgentMessage -> Process PConf PState ()
receive message = do
    target <- asks _target
    pieceArray <- asks _pieceArray

    case message of
        ReadBlock pieceNum block pieceV -> do
            debugP $ "reading block #" ++ show pieceNum ++ " " ++
                   "(" ++ show (_blockOffset block) ++ ", " ++ show (_blockSize block) ++ ")"
            pieceData <- liftIO $ readBlock target pieceNum block pieceArray
            liftIO . atomically $ putTMVar pieceV pieceData

        WriteBlock pieceNum block pieceData -> do
            debugP $ "writing block #" ++ show pieceNum ++ " " ++
                   "(" ++ show (_blockOffset block) ++ ", " ++ show (_blockSize block) ++ ")"
            liftIO $ writeBlock target pieceNum block pieceArray pieceData

        CheckPiece pieceNum checkV -> do
            let piece = pieceArray ! pieceNum
            checkResult <- liftIO $ FS.checkPiece target piece
            liftIO . atomically $ putTMVar checkV checkResult


