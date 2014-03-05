module Supervisor2
    ( RestartPolicy(..)
    , RestartStrategy(..)
    , ChildId
    , ChildType(..)
    , ChildSpec(..)
    , SupervisorMessage(..)
    ) where


import Data.Time.Clock
import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State hiding (state)

import qualified System.Timeout as T


import Server
import Process


data RestartPolicy
    = Permanent
    | Temporary
    | Transient

data RestartStrategy
    = OneForOne
    | OneForAll

type ChildId = String

data ChildType = Worker | Supervisor

data ChildSpec = ChildSpec
    { csType :: ChildType
    , csAction :: IO Reason
    , csRestart :: RestartPolicy
    , csShutdown :: IO ()
    , csShutdownTimeout :: Int
    }


data WorkerMessage
    = Dead ChildId Reason

data SupervisorMessage
    = Add ChildId ChildSpec
    | Stop ChildId
    | Delete ChildId
    | Restart ChildId
    | Terminate


data SupervisorState = SupervisorState
    { sStrategy :: RestartStrategy
    , sMaxRestart :: Int
    , sMaxRestartTime :: Int
    , sCrashTime :: [UTCTime]
    , sWorkerChan :: TChan WorkerMessage
    , sCommandChan :: TChan SupervisorMessage
    , sChildSpec :: M.Map ChildId ChildSpec
    , sChildThread :: M.Map ChildId ThreadId
    }


mkSupervisor :: RestartStrategy -> Int -> Int -> [(ChildId, ChildSpec)]
             -> IO SupervisorState
mkSupervisor strategy maxRestart maxRestartTime specs = do
    workerChan <- newTChanIO
    commandChan <- newTChanIO
    return $ SupervisorState
        { sStrategy = strategy
        , sMaxRestart = maxRestart
        , sMaxRestartTime = maxRestartTime
        , sCrashTime = []
        , sWorkerChan = workerChan
        , sCommandChan = commandChan
        , sChildSpec = M.fromList specs
        , sChildThread = M.empty
        }


start :: RestartStrategy -> Int -> Int -> [(ChildId, ChildSpec)]
      -> IO Reason
start strategy maxRestart maxRestartTime specs = do
    state <- mkSupervisor strategy maxRestart maxRestartTime specs
    runServer () state server
  where
    server = mkServer wait onMessage terminate


wait :: Process () SupervisorState (Either WorkerMessage SupervisorMessage)
wait = do
    workerChan <- gets sWorkerChan
    commandChan <- gets sCommandChan
    liftIO . atomically $
        (readTChan workerChan >>= return . Left) `orElse`
        (readTChan commandChan >>= return . Right)


onMessage :: Either WorkerMessage SupervisorMessage
          -> Process () SupervisorState (Maybe Reason)
onMessage message = do
    case message of
        Left (Dead cid reason) ->
            restartChild cid reason
        Right (Add cid spec) ->
            addChild cid spec >> return Nothing
        Right Terminate ->
            return $ Just Shutdown


startup :: Process () SupervisorState (Maybe Reason)
startup = do
    specs <- gets sChildSpec
    forM_ (M.assocs specs) $ \(id, spec) -> startChild id spec
    return Nothing


terminate :: Reason -> Process () SupervisorState ()
terminate _reason = do
    threads <- gets sChildThread
    forM_ (M.keys threads) shutdownChild


startChild :: ChildId -> ChildSpec -> Process () SupervisorState ()
startChild cid spec = do
    chan <- gets sWorkerChan
    thId <- liftIO $ forkFinally (csAction spec) (shutdown chan)
    modify $ \s -> s { sChildThread = M.insert cid thId $ sChildThread s }
  where
    shutdown chan (Left e) = shutdown chan $ Right (Exception e)
    shutdown chan (Right reason) = atomically $ writeTChan chan $ Dead cid reason


shutdownChild :: ChildId -> Process () SupervisorState ()
shutdownChild cid = do
    specs <- gets sChildSpec
    threads <- gets sChildThread
    let spec = M.lookup cid specs
        thread = M.lookup cid threads
    shutdownChild' spec thread
  where
    shutdownChild' (Just spec) (Just thread) = do
        _ <- liftIO $ T.timeout (csShutdownTimeout spec) (csShutdown spec)
        modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
        liftIO $ killThread thread
    shutdownChild' _ _ = return ()


restartChild :: ChildId -> Reason -> Process () SupervisorState (Maybe Reason)
restartChild _ Shutdown = return Nothing

restartChild cid reason = do
    crashes <- gets sCrashTime
    maxRestart <- gets sMaxRestart
    maxRestartTime <- gets sMaxRestartTime
    curtime <- liftIO getCurrentTime
    let crashes' = take (maxRestart + 1) (curtime : crashes)
    modify $ \state -> state { sCrashTime = crashes' }
    if needRestart maxRestart maxRestartTime crashes'
        then restartChild' cid reason
        else return $ Just reason


restartChild' :: ChildId -> Reason -> Process () SupervisorState (Maybe Reason)
restartChild' cid reason = do
    children <- gets sChildSpec
    case M.lookup cid children of
        Just spec ->
            if applyRestartPolicy (csRestart spec) reason
                then restartChild'' cid spec reason
                else modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
        _ -> return ()
    return Nothing


restartChild'' cid spec reason = do
    strategy <- gets sStrategy
    case strategy of
        OneForOne -> startChild cid spec
        OneForAll -> do
            chan <- liftIO newTChanIO
            modify $ \s -> s { sWorkerChan = chan }
            terminate reason
            startup
            return ()


needRestart :: Int -> Int -> [UTCTime] -> Bool
needRestart maxRestart maxRestartTime crashes
    | length crashes == 0 = True
    | maxRestart >= length crashes = True
    | otherwise = let
        hi = head crashes
        lo = last crashes
        in floor (diffUTCTime hi lo) > maxRestartTime


applyRestartPolicy :: RestartPolicy -> Reason -> Bool
applyRestartPolicy policy reason
    = case policy of
        Permanent -> True
        Temporary -> False
        Transient -> reason /= Normal


------

addChild :: ChildId -> ChildSpec -> Process () SupervisorState ()
addChild cid spec = do
    modify $ \s -> s { sChildSpec = M.insert cid spec (sChildSpec s) }
    startChild cid spec



