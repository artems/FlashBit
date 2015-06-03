module Main
    ( main
    ) where

import Data.List (find)

import Control.Monad (when, forM_)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random
import System.Environment (getArgs)
import System.Console.GetOpt

import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple

import ProcessGroup
import Torrent (mkPeerId, defaultPort)
import Version (version, protoVersion)

import FlashBit.Listen as Listen
import FlashBit.Console as Console
import FlashBit.PeerManager as PeerManager
import FlashBit.TorrentManager as TorrentManager
import FlashBit.PeerDatabase as PeerDatabase
import FlashBit.TorrentDatabase as TorrentDatabase


main :: IO ()
main = do
    args <- getArgs
    opts <- handleArgs args
    program opts

program :: ([Option], [String]) -> IO ()
program (opts, files)
    | Help `elem` opts    = putStrLn usageMessage
    | Version `elem` opts = printVersion
    | null files          = printNoTorrent
    | otherwise           = mainLoop opts files
  where
    printNoTorrent = putStrLn "No torrent file"

data Option = Version | Debug | Help
    deriving (Show, Eq)

options :: [OptDescr Option]
options =
    [ Option ['h', '?'] ["help"]    (NoArg Help)    "Выводит это сообщение"
    , Option ['d']      ["debug"]   (NoArg Debug)   "Печатает дополнительную информацию"
    , Option ['v']      ["version"] (NoArg Version) "Показывает версию программы"
    ]

getOption :: Option -> [Option] -> Maybe Option
getOption x = find (x ~=)
  where
    (~=) :: Option -> Option -> Bool
    Version ~= Version = True
    Debug   ~= Debug   = True
    Help    ~= Help    = True
    _       ~= _       = False

handleArgs :: [String] -> IO ([Option], [String])
handleArgs args = case getOpt Permute options args of
    (o, n, []) -> return (o, n)
    (_, _, er) -> error $ concat er ++ "\n" ++ usageMessage

usageMessage :: String
usageMessage = usageInfo header options
  where
    header = "Usage: FlashBit [option...] FILE"

printVersion :: IO ()
printVersion = putStrLn $ "FlashBit version " ++ version ++ "\n"

setupLogging :: [Option] -> IO ()
setupLogging opts = do
    logStream <- streamHandler stdout DEBUG >>= \logger ->
        return $ setFormatter logger $
            tfLogFormatter "%F %T" "[$time] $prio $loggername: $msg"
    when (Debug `elem` opts) $ do
        updateGlobalLogger rootLoggerName $
            (setHandlers [logStream]) . (setLevel DEBUG)

mainLoop :: [Option] -> [String] -> IO ()
mainLoop opts files = do
    debugM "Main" "Инициализация"
    setupLogging opts
    stdGen <- newStdGen
    let peerId = mkPeerId stdGen protoVersion
    debugM "Main" $ "Сгенерирован peer_id: " ++ peerId

    torrentChan      <- newTChanIO
    peerManagerChan  <- newTChanIO
    peerDatabase     <- atomically mkPeerDatabaseSTM
    torrentDatabase  <- atomically mkTorrentDatabaseSTM

    let addTorrent = atomically . writeTChan torrentChan . TorrentManager.AddTorrent
    forM_ files addTorrent

    group <- initGroup
    let actions =
            [ runConsole torrentChan torrentDatabase
            , runPeerManager peerId peerDatabase torrentDatabase peerManagerChan
            , runTorrentManager peerId peerDatabase torrentDatabase peerManagerChan torrentChan
            , runListen defaultPort peerManagerChan
            ]
    runGroup group actions >>= exitStatus

    debugM "Main" "Выход"

exitStatus :: Either SomeException () -> IO ()
exitStatus (Left (SomeException e)) = print e
exitStatus _                        = return ()
