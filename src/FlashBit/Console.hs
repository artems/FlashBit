module FlashBit.Console
    ( runConsole
    ) where

import Data.List
import Control.Monad.Trans (lift)
import Control.Monad.Reader (liftIO, asks)
import Control.Concurrent
import Control.Concurrent.STM
import System.Console.Haskeline

import Process
import Torrent
import FlashBit.TorrentManager.Chan (TorrentManagerMessage)
import qualified FlashBit.TorrentManager.Chan as TorrentManager
import FlashBit.TorrentDatabase (TorrentDatabaseTVar)
import qualified FlashBit.TorrentDatabase as TorrentDatabase


data Command
    = Quit           -- ^ Quit the program
    | Show           -- ^ Show the current state
    | Help           -- ^ Print the help message
    | Unknown String -- ^ Unknown command
    deriving (Eq, Show)

data PConf = PConf
    { _torrentDatabase :: TorrentDatabaseTVar
    , _torrentChan     :: TChan TorrentManagerMessage
    }

instance ProcessName PConf where
    processName _ = "Console"

type PState = ()


runConsole :: TorrentDatabaseTVar -> TChan TorrentManagerMessage -> IO ()
runConsole torrentDatabase torrentChan = do
    let pconf = PConf torrentDatabase torrentChan
    let pstate = ()
    wrapProcess pconf pstate process

process :: Process PConf PState ()
process = runInputT defaultSettings loop
  where
    loop = do
        userInput <- getInputLine "% "
        case userInput of
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
            stats <- liftIO . atomically $
                TorrentDatabase.getStatisticSTM torrentDatabase
            liftIO . putStrLn . intercalate " " $ map showTorrent stats

        Help -> do
            liftIO . putStrLn $ helpMessage

        Unknown line -> do
            liftIO . putStrLn $ "Uknown command: " ++ show line
  where
    percentage :: TorrentStatus -> Integer
    percentage stat =
        let left = fromIntegral (_torrentLeft stat)
            size = fromIntegral (_torrentSize stat)
            in 100 - round (left / size * 100 :: Double)
    showTorrent (infoHash, stat) =
        showInfoHash infoHash ++
        " %: " ++  show (percentage stat) ++
        " uploaded: " ++ show (_torrentUploaded stat) ++
        " downloaded: " ++ show (_torrentDownloaded stat)
