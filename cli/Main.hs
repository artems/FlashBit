module Main
    ( main
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.IO
import System.Random


import Process
import Process.Console as Console
-- import Process.Status as Status
-- import Process.TorrentManager as TorrentManager
-- import Process.TorrentManagerChan (TorrentManagerMessage(AddTorrent))
import Torrent
import Protocol.Types
import Version (version)
import Server
import Supervisor


main :: IO ()
main = do
    args <- getArgs
    opts <- handleArgs args
    run opts


printVersion :: IO ()
printVersion = putStrLn $ "PROGRAM version " ++ version ++ "\n"


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
    Debug ~= Debug = True
    Help ~= Help = True
    _ ~= _ = False


handleArgs :: [String] -> IO ([Option], [String])
handleArgs args = case getOpt Permute options args of
    (o, n, [] ) -> return (o, n)
    (_, _, err) -> error (concat err ++ usageMessage)


usageMessage = usageInfo header options
  where
    header = "Usage: PROGRAM [option...] FILE"


run :: ([Option], [String]) -> IO ()
run (opts, files) =
    if showHelp then putStrLn usageMessage
    else if showVersion then printVersion
        else if null files then putStrLn "No torrent file"
            else download opts files
  where
    showHelp = Help `elem` opts
    showVersion = Version `elem` opts


setupLogging :: [Option] -> IO ()
setupLogging opts = do
    logStream <- streamHandler stdout DEBUG >>= \l -> return $
        setFormatter l (tfLogFormatter "%F %X" "[$time] $prio $loggername: $msg")
    when True $
    -- when (Debug `elem` opts) $
        updateGlobalLogger rootLoggerName $
            (setHandlers [logStream]) . (setLevel DEBUG)


download :: [Option] -> [String] -> IO ()
download opts files = do
    setupLogging opts

    debugM "Main" "Инициализация"
    peerId <- newStdGen >>= (return . mkPeerId)
    debugM "Main" $ "Присвоен peer_id: " ++ peerId
    statusTV <- newTVarIO []
    statusChan <- newTChanIO
    torrentChan <- newTChanIO

    waitMutex <- newEmptyTMVarIO

    runDownload waitMutex
    -- _ <- Status.start statusTV statusChan
    -- _ <- Console.start waitMutex statusChan
    -- _ <- TorrentManager.start peerId statusTV statusChan torrentChan

    -- atomically $ writeTChan torrentChan (map AddTorrent files)
    -- atomically $ takeTMVar waitMutex
    debugM "Main" "Завершаем работу"
    threadDelay (1000)
    return ()


runDownload :: TMVar () -> IO Reason
runDownload waitMutex = do
    superChan <- newTChanIO
    let specs =
            [ ("console", Console.specConsole waitMutex superChan)
            ]
    runSupervisor OneForOne 5 60 superChan specs

