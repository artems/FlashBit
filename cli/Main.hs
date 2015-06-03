{-# LANGUAGE CPP #-}

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
import FlashBit.TorrentManager.Chan as TorrentManager
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

programName :: String
programName = "flashbit"

usageMessage :: String
usageMessage = usageInfo header options
  where
    header = "Usage: " ++ programName ++ " [OPTIONS...] FILE"

printVersion :: IO ()
printVersion = putStrLn $ programName ++ " version " ++ version ++ "\n"

setupLogging :: [Option] -> IO ()
setupLogging opts = do
    logStream <- streamHandler stdout NOTICE >>= \logger -> return $
        setFormatter logger $
            tfLogFormatter "%F %T" "[$time] $prio $loggername: $msg"
    when (Debug `elem` opts) $ do
        updateGlobalLogger rootLoggerName $
            (setHandlers [logStream]) . (setLevel NOTICE)

mainLoop :: [Option] -> [String] -> IO ()
mainLoop opts files = do
    debugM "Main" "Initialization"
    setupLogging opts

    stdGen <- newStdGen
    let peerId = mkPeerId stdGen protoVersion
    debugM "Main" $ "Generated peer_id: " ++ peerId

    peerManagerChan    <- newTChanIO
    torrentManagerChan <- newTChanIO
    peerDatabase       <- atomically mkPeerDatabaseSTM
    torrentDatabase    <- atomically mkTorrentDatabaseSTM

    forM_ files (addTorrent torrentManagerChan)

    group <- initGroup
    let actions =
            [ runListen defaultPort peerManagerChan
            , runConsole torrentDatabase torrentManagerChan
            , runPeerManager
                peerId
                peerDatabase
                torrentDatabase -- used for searching info_hash after handshake
                peerManagerChan
            , runTorrentManager
                peerId          -- used by a tracker
                peerDatabase    -- used by a choke manager
                torrentDatabase
                peerManagerChan -- used for sending peers from tracker
                torrentManagerChan
            ]
    runGroup group actions >>= exit

    debugM "Main" "Exit"
  where
    addTorrent chan torrentFile = atomically . writeTChan chan $
        TorrentManager.AddTorrent torrentFile "." True

exit :: Either SomeException () -> IO ()
exit (Left (SomeException e)) = print e
exit _                        = return ()
