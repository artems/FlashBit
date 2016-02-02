module Console
    ( console
    ) where

import Data.List
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline
import Torrent

import FlashBit.API as API


data Command
    = Add FilePath   -- ^ Add new torrent
    | Stop String    -- ^ Stop downloading
    | Start String   -- ^ Start downloading
    | Show           -- ^ Show the current state
    | Help           -- ^ Print the help message
    | Quit           -- ^ Quit the program
    | None           -- ^ Do nothing
    | Unknown String -- ^ Unknown command
    deriving (Eq, Show)

console :: API -> IO ()
console api = runInputT defaultSettings loop
  where
    loop = do
        userInput <- getInputLine "% "
        continue  <- case userInput of
            Nothing    -> return True
            Just input -> liftIO $ receive api (parseCommand input)
        when continue loop

parseCommand :: String -> Command
parseCommand cmd = case cmd of
    ""     -> None
    "help" -> Help
    "quit" -> Quit
    "show" -> Show
    ('a' : 'd' : 'd' : ' ' : filepath) -> Add filepath
    ('s' : 't' : 'o' : 'p' : ' ' : infoHash) -> Stop infoHash
    ('s' : 't' : 'a' : 'r' : 't' : ' ' : infoHash) -> Start infoHash
    line   -> Unknown line

helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help    - Show this help\n"
    , "  quit    - Quit the program\n"
    , "  show    - Show the current downloading status\n"
    ]

receive :: API -> Command -> IO Bool
receive api command = do
    case command of
        Add filepath -> do
            let filepath' = normalizeFilepath filepath
            API.addTorrent api filepath' False
            return True

        Stop infoHash -> do
            let infoHash' = readInfoHash infoHash
            API.stopTorrent api infoHash'
            return True

        Start infoHash -> do
            let infoHash' = readInfoHash infoHash
            API.startTorrent api infoHash'
            return True

        Show -> do
            stats <- API.getStatistic api
            liftIO . putStrLn . intercalate "\n" $ map showTorrent stats
            return True

        Help -> do
            liftIO . putStrLn $ helpMessage
            return True

        Quit -> do
            API.shutdown api
            return False

        None -> return True

        Unknown line -> do
            liftIO . putStrLn $ "Uknown command: " ++ show line
            return True
  where
    normalizeFilepath :: FilePath -> FilePath
    normalizeFilepath =
        reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    percentage :: Torrent -> TorrentStatus -> Integer
    percentage torrent stat =
        let left = fromIntegral (_torrentLeft stat)
            size = fromIntegral (_torrentLength torrent)
            in 100 - round (left / size * 100 :: Double)

    showTorrent :: (InfoHash, Torrent, TorrentStatus) -> String
    showTorrent (infoHash, torrent, stat) =
        showInfoHash infoHash ++
        " " ++ (if _torrentIsActive stat then ">>" else "||") ++
        " " ++ show (_torrentName torrent) ++
        " %: " ++  show (percentage torrent stat) ++
        " uploaded: " ++ show (_torrentUploaded stat) ++
        " downloaded: " ++ show (_torrentDownloaded stat)
