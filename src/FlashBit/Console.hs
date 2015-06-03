module FlashBit.Console
    ( runConsole
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift, liftIO)
import System.Console.Haskeline

import Process
import Torrent
import qualified FlashBit.TorrentManager as TorrentManager
import qualified FlashBit.TorrentDatabase as TorrentDatabase


data Command
    = Quit           -- ^ Quit the program
    | Show           -- ^ Show the current state
    | Help           -- ^ Print the help message
    | Unknown String -- ^ Unknown command
    deriving (Eq, Show)

data PConf = PConf
    { _torrentChan     :: TChan TorrentManager.TorrentManagerMessage
    , _torrentDatabase :: TorrentDatabase.TorrentDatabaseTVar
    }

instance ProcessName PConf where
    processName _ = "Console"

type PState = ()


runConsole :: TChan TorrentManager.TorrentManagerMessage
           -> TorrentDatabase.TorrentDatabaseTVar
           -> IO ()
runConsole torrentChan torrentDatabase = do
    let pconf = PConf torrentChan torrentDatabase
        pstate = ()
    wrapProcess pconf pstate process

process :: Process PConf PState ()
process = runInputT defaultSettings loop
  where
    loop = do
        uinput   <- getInputLine "% "
        case uinput of
            Nothing    -> return ()
            Just input -> lift . receive $ parseCommand input
        loop

parseCommand :: String -> Command
parseCommand cmd = case cmd of
    "help" -> Help
    "quit" -> Quit
    "show" -> Show
    line   -> Unknown line

helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help    - Show this help\n"
    , "  quit    - Quit the program\n"
    , "  show    - Show the current downloading status\n"
    ]

receive :: Command -> Process PConf PState ()
receive command = do
    torrentChan     <- asks _torrentChan
    torrentDatabase <- asks _torrentDatabase

    case command of
        Quit -> do
            waitV <- liftIO newEmptyMVar
            let message = TorrentManager.Shutdown waitV
            liftIO . atomically $ writeTChan torrentChan message
            liftIO $ takeMVar waitV
            stopProcess

        Show -> do
            stats <- liftIO . atomically $ TorrentDatabase.getStatisticSTM torrentDatabase
            liftIO . putStrLn . show $ map (\(i, s) -> (showInfoHash i, s)) stats

        Help -> do
            liftIO . putStrLn $ helpMessage

        Unknown line -> do
            liftIO . putStrLn $ "Uknown command: " ++ show line
