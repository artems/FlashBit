module Process.Console
    ( runConsole
    , specConsole
    ) where


import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Reader (liftIO, asks)

import Server
import Process
import Supervisor
-- import qualified Process.Status


data PConf = PConf
    { cCmdChan :: TChan Command
    , cOutChan :: TChan String
    , cWaitMutex :: TMVar ()
    }

data Command
    = Quit
    | Show
    | Help
    | Unknown String
    deriving (Eq, Show)


runConsole :: TMVar () -> IO Reason
runConsole waitMutex = do
    cmdChan <- newTChanIO
    outChan <- newTChanIO
    let specs =
            [ ("reader", specReader cmdChan)
            , ("writer", specWriter outChan)
            , ("handler", specHandler cmdChan outChan waitMutex)
            ]
    runSupervisor OneForOne 3 60 specs


specConsole :: TMVar () -> IO ChildSpec
specConsole waitMutex = do
    return $ ChildSpec
        { csType = Worker
        , csAction = runConsole waitMutex
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 100
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


handler :: TChan Command -> TChan String -> TMVar () -> IO Reason
handler cmdChan outChan waitMutex = runServer conf () startup server
  where
    conf = PConf cmdChan outChan waitMutex
    server = mkServer wait receive terminate
    startup = return ()
    terminate = \_ -> return ()


wait :: Process PConf () Command
wait = do
    cmdChan <- asks cCmdChan
    liftIO . atomically $ readTChan cmdChan


receive :: Command -> Process PConf () ()
receive cmd = do
    outChan <- asks cOutChan
    waitMutex <- asks cWaitMutex
    case cmd of
        Quit -> liftIO . atomically $ putTMVar waitMutex ()
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
        , csShutdownTimeout = 100
        }


specWriter :: TChan String -> IO ChildSpec
specWriter outChan = do
    return $ ChildSpec
        { csType = Worker
        , csAction = writer outChan
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 100
        }


specHandler :: TChan Command -> TChan String -> TMVar () -> IO ChildSpec
specHandler cmdChan outChan waitMutex = do
    return $ ChildSpec
        { csType = Worker
        , csAction = handler cmdChan outChan waitMutex
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 100
        }

