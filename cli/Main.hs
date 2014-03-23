{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


import Data.List (find)

import Control.Monad (forM_)
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

import Torrent (mkPeerId, defaultPort)
import Version (version, protoVersion)
import ProcessGroup


import Process.Status
import Process.Listen
import Process.Console
import Process.PeerManager
import Process.ChokeManager
import Process.TorrentManager


main :: IO ()
main = do
    args <- getArgs
    opts <- handleArgs args
    program opts


program :: ([Option], [String]) -> IO ()
program (opts, files) =
    if showHelp then putStrLn usageMessage
    else if showVersion then printVersion
        else if null files then printNoTorrent
            else download opts files
  where
    showHelp = Help `elem` opts
    showVersion = Version `elem` opts
    printNoTorrent = putStrLn "No torrent file"


data Option
    = Version
    | Debug
    | Help
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
    header = "Usage: PROGRAM [option...] FILE"


printVersion :: IO ()
printVersion = putStrLn $ "PROGRAM version " ++ version ++ "\n"


setupLogging :: [Option] -> IO ()
setupLogging _opts = do
    logStream <- streamHandler stdout DEBUG >>= \logger ->
        return $ setFormatter logger $
            tfLogFormatter "%F %T" "[$time] $prio $loggername: $msg"
    updateGlobalLogger rootLoggerName $
        (setHandlers [logStream]) . (setLevel DEBUG)


download :: [Option] -> [String] -> IO ()
download opts files = do
    setupLogging opts

    debugM "Main" "Инициализация"

    peerId <- newStdGen >>= (return . mkPeerId protoVersion)
    debugM "Main" $ "Присвоен peer_id: " ++ peerId

    rateV   <- newTVarIO []
    statusV <- newTVarIO []
    peerMChan    <- newTChanIO
    chokeMChan   <- newTChanIO
    statusChan   <- newTChanIO
    torrentMChan <- newTChanIO

    forM_ files (atomically . writeTChan torrentMChan . AddTorrent)

    let allForOne =
            [ runStatus statusV statusChan
            , runConsole statusChan
            , runPeerManager peerId rateV peerMChan chokeMChan
            , runChokeManager rateV chokeMChan
            , runTorrentManager peerId statusV torrentMChan statusChan peerMChan chokeMChan
            , runListen defaultPort peerMChan
            ]

    group <- initGroup
    result <- runGroup group allForOne
    case result of
        Left (e :: SomeException) ->
            print e
        _ -> return ()

    debugM "Main" "Завершаем работу"
    threadDelay $ 500 * 1000
    return ()


