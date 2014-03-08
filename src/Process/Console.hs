module Process.Console
    ( runConsole
    , specConsole
    ) where


import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Reader (liftIO, asks)

import System.Log.Logger

import Server
import Process
import Supervisor
-- import qualified Process.Status


data PConf = PConf
    { cCmdChan :: TChan Command
    , cOutChan :: TChan String
    , cWaitMutex :: TMVar ()
    , cSuperChan :: TChan SupervisorMessage
    }

data Command
    = Quit
    | Show
    | Help
    | Unknown String
    deriving (Eq, Show)


runConsole :: TMVar () -> TChan SupervisorMessage -> TChan SupervisorMessage -> IO Reason
runConsole waitMutex superChan consoleChan = do
    cmdChan <- newTChanIO
    outChan <- newTChanIO
    let specs =
            [ ("reader", specReader cmdChan)
            , ("writer", specWriter outChan)
            , ("handler", specHandler cmdChan outChan waitMutex superChan)
            ]
    runSupervisor OneForOne 3 60 consoleChan specs


specConsole :: TMVar () -> TChan SupervisorMessage -> IO ChildSpec
specConsole waitMutex superChan = do
    consoleChan <- newTChanIO
    return $ ChildSpec
        { csType = Worker
        , csAction = runConsole waitMutex superChan consoleChan
        , csRestart = Permanent
        , csShutdown = atomically $ writeTChan consoleChan Terminate
        , csShutdownTimeout = 1000
        }


helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help  - Show this help\n"
    , "  quit  - Quit the program\n"
    , "  show  - Show the current downloading status\n"
    ]


reader :: TChan Command -> IO Reason
reader cmdChan = do
    _ <- forever $ do
        line <- getLine
        atomically $ writeTChan cmdChan (command line)
    return Normal
  where
    command line = case line of
        "help" -> Help
        "quit" -> Quit
        "show" -> Show
        cmd    -> Unknown cmd


writer :: TChan String -> IO Reason
writer outChan = do
    _ <- forever $ do
        message <- atomically . readTChan $ outChan
        putStrLn message
    return Normal


handler :: TChan Command -> TChan String -> TMVar () -> TChan SupervisorMessage -> IO Reason
handler cmdChan outChan waitMutex superChan = runServer conf () startup server
  where
    conf = PConf cmdChan outChan waitMutex superChan
    server = mkServer wait receive terminate
    -- startup = return ()
    startup = liftIO $ debugM "Console" "Процесс запущен"
    -- terminate = \_ -> return ()
    terminate reason = liftIO $ debugM "Console" ("Процесс остановлен по причине " ++ show reason)


wait :: Process PConf () Command
wait = do
    cmdChan <- asks cCmdChan
    liftIO . atomically $ readTChan cmdChan


receive :: Command -> Process PConf () ()
receive cmd = do
    outChan <- asks cOutChan
    waitMutex <- asks cWaitMutex
    superChan <- asks cSuperChan
    case cmd of
        Quit ->
            -- liftIO . atomically $ putTMVar waitMutex ()
            liftIO . atomically $ writeTChan superChan Terminate
        Help -> liftIO . atomically $ writeTChan outChan helpMessage
        Show -> do
            -- ret <- liftIO newEmptyTMVarIO
            -- liftIO . atomically $ writeTChan statusChan (RequestAllTorrents ret)
            -- status <- liftIO . atomically $ takeTMVar ret
            let status = "no status"
            liftIO . atomically $ writeTChan outChan (show status)
        (Unknown str) -> liftIO . atomically $ writeTChan outChan $ "Неизвестная комманда: " ++ str


specReader :: TChan Command -> IO ChildSpec
specReader cmdChan = do
    return $ ChildSpec
        { csType = Worker
        , csAction = reader cmdChan
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 0
        }


specWriter :: TChan String -> IO ChildSpec
specWriter outChan = do
    return $ ChildSpec
        { csType = Worker
        , csAction = writer outChan
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 0
        }


specHandler :: TChan Command -> TChan String -> TMVar () -> TChan SupervisorMessage -> IO ChildSpec
specHandler cmdChan outChan waitMutex superChan = do
    return $ ChildSpec
        { csType = Worker
        , csAction = handler cmdChan outChan waitMutex superChan
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 0
        }

