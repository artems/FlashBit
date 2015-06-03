module FlashBit.FileAgent
    ( runFileAgent
    , FileAgentMessage(..)
    ) where

import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)
import Data.Array ((!))
import qualified Data.ByteString as B

import Process
import Torrent
import Torrent.File


data FileAgentMessage
    = ReadBlock PieceNum PieceBlock (TMVar B.ByteString)
    | WriteBlock PieceNum PieceBlock B.ByteString
    | CheckPiece PieceNum (TMVar Bool)

data PConf = PConf
    { _target        :: FileRec
    , _infoHash      :: InfoHash
    , _pieceArray    :: PieceArray
    , _fileAgentChan :: TChan FileAgentMessage
    }

instance ProcessName PConf where
    processName pconf = "FileAgent [" ++ showInfoHash (_infoHash pconf) ++ "]"

type PState = ()


runFileAgent :: FileRec -> InfoHash -> PieceArray -> TChan FileAgentMessage -> IO ()
runFileAgent target infoHash pieceArray fileAgentChan = do
    let pconf = PConf target infoHash pieceArray fileAgentChan
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
        ReadBlock pieceNum block pieceV -> do
            {-
            debugP $ "reading block #" ++ show pieceNum ++ " " ++ "(" ++
                   "offset=" ++ show (_blockOffset block) ++ ", " ++
                   "length=" ++ show (_blockLength block) ++
                   ")"
            -}
            let piece = pieceArray ! pieceNum
            pieceData <- liftIO $ readBlock target piece block
            liftIO . atomically $ putTMVar pieceV pieceData

        WriteBlock pieceNum block pieceData -> do
            {-
            debugP $ "writing block #" ++ show pieceNum ++ " " ++ "(" ++
                   "offset=" ++ show (_blockOffset block) ++ ", " ++
                   "length=" ++ show (_blockLength block) ++
                   ")"
            -}
            let piece = pieceArray ! pieceNum
            liftIO $ writeBlock target piece block pieceData

        CheckPiece pieceNum checkV -> do
            -- debugP $ "checking piece #" ++ show pieceNum
            let piece = pieceArray ! pieceNum
            result <- liftIO $ checkPiece target piece
            liftIO . atomically $ putTMVar checkV result
