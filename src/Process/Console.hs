module Process.Console
    ( runConsole
    ) where


import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

import Process
import Process.Status


data Command
    = Quit           -- ^ Quit the program
    | Show           -- ^ Show current state
    | Help           -- ^ Print Help message
    | Unknown String -- ^ Unknown command
    deriving (Eq, Show)


data PConf = PConf
    { _statusChan  :: TChan StatusMessage
    }

instance ProcessName PConf where
    processName _ = "Console"

type PState = ()


runConsole :: TChan StatusMessage -> IO ()
runConsole statusChan = do
    let pconf  = PConf statusChan
    wrapProcess pconf () process


process :: Process PConf PState ()
process = do
    message <- getCommand `fmap` liftIO getLine
    receive message
    process
  where
    getCommand line = case line of
        "help" -> Help
        "quit" -> Quit
        "show" -> Show
        input  -> Unknown input


receive :: Command -> Process PConf PState ()
receive command = do
    statusChan <- asks _statusChan

    case command of
        Quit ->
            stopProcess
        Show -> do
            statsV <- liftIO newEmptyTMVarIO
            liftIO . atomically $ writeTChan statusChan $
                RequestStatistic statsV
            stats  <- liftIO . atomically $ takeTMVar statsV
            liftIO . putStrLn $ show stats
        Help ->
            liftIO . putStrLn $ helpMessage
        Unknown line ->
            liftIO . putStrLn $ "Uknown command: " ++ show line


helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help    - Show this help\n"
    , "  quit    - Quit the program\n"
    , "  show    - Show the current downloading status\n"
    ]


