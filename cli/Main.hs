module Main
    ( main
    , getOption
    ) where


import Data.List (find)

import Control.Monad (forM_)
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

import Protocol
import Torrent (mkPeerId)
import Version (version)

import Platform.Process

import Process
import qualified Process.Status as Status
import qualified Process.Console as Console
import qualified Process.TorrentManager as TorrentManager


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
    (o, n, [] ) -> return (o, n)
    (_, _, err) -> error (concat err ++ "\n" ++ usageMessage)


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

    peerId <- newStdGen >>= (return . mkPeerId)
    debugM "Main" $ "Присвоен peer_id: " ++ peerId

    _ <- runDownload peerId files

    debugM "Main" "Завершаем работу"
    threadDelay $ 1000 * 1000
    return ()


runDownload :: PeerId -> [FilePath] -> IO Reason
runDownload peerId files = do
    statusTV <- newTVarIO []
    superChan <- newTChanIO
    statusChan <- newTChanIO
    consoleChan <- newTChanIO
    torrentChan <- newTChanIO
    torrentManagerChan <-newTChanIO

    forM_ files $ \file ->
        atomically $ writeTChan torrentManagerChan (TorrentManager.AddTorrent file)

    let status = specServer2 statusChan Status.Terminate
            (Status.runStatus statusTV statusChan)
        torrentManager = specServer2 torrentManagerChan TorrentManager.Terminate
            (TorrentManager.runTorrentManager peerId statusTV statusChan torrentChan torrentManagerChan)
        specs =
            [ ("console", specSupervisor (Console.runConsole superChan statusChan consoleChan) consoleChan)
            , ("status", status)
            , ("torrent manager", torrentManager)
            , ("torrent supervisor", specSupervisor (runTorrentSupervisor torrentChan) torrentChan)
            ]
    supervisor0 "Main" superChan specs
  where
    runTorrentSupervisor chan = supervisor0 "TorrentSupervisor" chan []


