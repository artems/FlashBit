module Main (main) where

import Data.Word
import Data.Bits ((.&.), shiftL, shiftR)
import Data.Char (digitToInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket (Socket)
import qualified Network.Socket as S

import Torrent
import qualified Torrent.Message as TM

import FS
import Digest
import Process
import Process.Peer
import Process.PieceManager
import Process.FileAgent
import Process.Channel

import System.IO
import System.Random
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple


peerId = "-AT00-123asd213dfrae"
infohash = B.pack
    [106, 54, 222, 32, 29, 242, 241, 178, 200, 23, 71, 76, 48, 117, 255, 14, 170, 140, 119, 133]

main = do
    hSetBuffering stdout LineBuffering
    setupLogging
    run "127.0.0.1:61169"

connect :: S.SockAddr -> IO Socket
connect peer = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect sock peer
    return sock

createSocket addr =
    let [host, port] = splitWhen (== ':') addr
        port1 = (read port :: Word16)
        port2 = ((port1 .&. 0x00FF) `shiftL` 8) + ((port1 .&. 0xFF00) `shiftR` 8)
        host1 = convert host
     in S.SockAddrInet (S.PortNum port2) host1


openTFile torrentFile = do
    bcAttempt <- openTorrent torrentFile
    case bcAttempt of
        Right bc -> case mkTorrent digest bc of
            Just torrent -> do
                file <- openAndCheckFile bc
                return (torrent, file)
            Nothing ->
                parseFailure
        Left msg ->
            openFailure msg
  where
    openFailure msg = do
        putStrLn $ "Не удается открыть torrent-файл " ++ torrentFile
        putStrLn $ msg
        error "."
    parseFailure = do
        putStrLn $ "Не удается прочитать torrent-файл " ++ torrentFile
        error "."

run addr = do
    let peer = createSocket addr
        torrentFile = "assets/ubuntu-13.10-server-amd64.iso.torrent"
    peerId          <- newStdGen >>= (return . mkPeerId "0100")
    socket          <- connect peer
    rateV           <- newTVarIO []
    statV           <- newTVarIO []
    pieceMChan      <- newTChanIO
    fileAgentChan   <- newTChanIO
    peerNetworkChan <- newTChanIO
    statusChan      <- newTChanIO
    chokeMChan      <- newTChanIO
    (torrent, (target, pieceArray, pieceHaveMap)) <- openTFile torrentFile
    let numPieces = _torrentPieceCount torrent
    _ <- forkIO $ do
            (Connect infoHash threadId fromChan) <- atomically $ readTChan peerNetworkChan
            return ()
    _ <- forkIO $ do
            runPieceManager infohash pieceArray pieceHaveMap pieceMChan fileAgentChan statusChan chokeMChan
    _ <- forkIO $ do
            runFileAgent target pieceArray fileAgentChan
    runPeer socket infohash peerId [] pieceArray numPieces rateV statV fileAgentChan peerNetworkChan pieceMChan


setupLogging :: IO ()
setupLogging = do
    logStream <- streamHandler stdout DEBUG >>= \logger ->
        return $ setFormatter logger $
            tfLogFormatter "%F %T" "[$time] $prio $loggername: $msg"
    updateGlobalLogger rootLoggerName $
        (setHandlers [logStream]) . (setLevel DEBUG)


toInt :: String -> Int
toInt s = foldl (\acc c -> acc * 10 + digitToInt c) 0 s

convert :: (Integral a) => String -> a
convert host = fromIntegral . collect . map toInt . splitWhen (== '.') $ host

collect :: [Int] -> Int
collect [a4, a3, a2, a1] = shiftL a1 24 + shiftL a2 16 + shiftL a3 8 + a4

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s
    = case dropWhile p s of
        "" -> []
        s' -> w : splitWhen p s''
            where (w, s'') = break p s'


