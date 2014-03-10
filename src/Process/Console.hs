module Process.Console
    ( runConsole
    ) where


import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, ask, asks)

import Platform.Process
import Platform.Supervisor

import Process
import qualified Process.Status as Status


data PConf = PConf
    { cCmdChan :: TChan Command
    , cOutChan :: TChan String
    , cSuperChan :: TChan SupervisorMessage
    , cStatusChan :: TChan Status.StatusMessage
    }

data Command
    = Quit
    | Show
    | Help
    | Unknown String
    deriving (Eq, Show)


runConsole :: TChan SupervisorMessage -> TChan Status.StatusMessage -> TChan SupervisorMessage -> IO Reason
runConsole superChan statusChan consoleChan = do
    cmdChan <- newTChanIO
    outChan <- newTChanIO
    let specs =
            [ ("reader", specServer (reader cmdChan))
            , ("writer", specServer (writer outChan))
            , ("handler", specServer (handler cmdChan outChan superChan statusChan))
            ]
    supervisor0 "Console" consoleChan specs


reader :: TChan Command -> IO Reason
reader cmdChan = process0 "Console.Reader" cmdChan () wait receive
  where
    wait = liftIO getLine
    receive line = do
        chan <- ask
        liftIO . atomically $ writeTChan chan $ command line
    command line = case line of
        "help" -> Help
        "quit" -> Quit
        "show" -> Show
        cmd    -> Unknown cmd


writer :: TChan String -> IO Reason
writer outChan = process0 "Console.Writer" outChan () wait receive
  where
    wait = liftIO . atomically . readTChan =<< ask
    receive message = liftIO . putStrLn $ message


handler :: TChan Command
        -> TChan String
        -> TChan SupervisorMessage
        -> TChan Status.StatusMessage
        -> IO Reason
handler cmdChan outChan superChan statusChan =
    process0 "Console.Handler" conf () wait receive
  where
    conf = PConf cmdChan outChan superChan statusChan
    wait = do
        chan <- asks cCmdChan
        liftIO . atomically $ readTChan chan
    receive cmd = handleCommand cmd


handleCommand :: Command -> Process PConf () ()
handleCommand cmd = do
    outChan <- asks cOutChan
    superChan <- asks cSuperChan
    statusChan <- asks cStatusChan
    case cmd of
        Quit ->
            liftIO . atomically $ writeTChan superChan Terminate
        Help -> liftIO . atomically $ writeTChan outChan helpMessage
        Show -> do
            ret <- liftIO newEmptyTMVarIO
            liftIO . atomically $ writeTChan statusChan (Status.RequestAllTorrents ret)
            status <- liftIO . atomically $ takeTMVar ret
            liftIO . atomically $ writeTChan outChan (show status)
        (Unknown str) -> liftIO . atomically $ writeTChan outChan $ "Неизвестная комманда: " ++ str


helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help  - Show this help\n"
    , "  quit  - Quit the program\n"
    , "  show  - Show the current downloading status\n"
    ]


