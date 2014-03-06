module Supervisor
    ( ChildId
    , ChildType(..)
    , ChildSpec(..)
    , RestartPolicy(..)
    , RestartStrategy(..)
    , SupervisorMessage(..)
    , runSupervisor
    ) where


import Data.Time.Clock
import qualified Data.Map as M
import Data.Maybe (isNothing)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
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
    = Add ChildId (IO ChildSpec)
    | Stop ChildId
    | Delete ChildId
    | Restart ChildId
    | Terminate


data SupervisorState = SupervisorState
    { sStrategy         :: RestartStrategy
    , sMaxRestart       :: Int
    , sMaxRestartTime   :: Int
    , sCrashTime        :: [UTCTime]
    , sCommandChan      :: TChan SupervisorMessage
    , sChildSpec        :: M.Map ChildId (IO ChildSpec)
    , sChildThread      :: M.Map ChildId (ThreadId, TMVar Reason, ChildSpec)
    }


mkSupervisor :: RestartStrategy -> Int -> Int -> [(ChildId, IO ChildSpec)]
             -> IO SupervisorState
mkSupervisor strategy maxRestart maxRestartTime specs = do
    commandChan <- newTChanIO
    return $ SupervisorState
        { sStrategy = strategy
        , sMaxRestart = maxRestart
        , sMaxRestartTime = maxRestartTime
        , sCrashTime = []
        , sCommandChan = commandChan
        , sChildSpec = M.fromList specs
        , sChildThread = M.empty
        }


runSupervisor :: RestartStrategy -> Int -> Int -> [(ChildId, IO ChildSpec)]
      -> IO Reason
runSupervisor strategy maxRestart maxRestartTime specs = do
    state <- mkSupervisor strategy maxRestart maxRestartTime specs
    runServer () state startup server
  where
    server = mkServer wait onMessage terminate


wait :: Process () SupervisorState (Either WorkerMessage SupervisorMessage)
wait = do
    threads <- gets sChildThread
    commandChan <- gets sCommandChan
    let command = readTChan commandChan >>= return . Right
        waitMessage = foldl myfold command (M.assocs threads)
    liftIO . atomically $ waitMessage
  where
    myfold stm (cid, (_, stop, _)) = stm `orElse`
        (takeTMVar stop >>= return . Left . Dead cid)


onMessage :: Either WorkerMessage SupervisorMessage
          -> Process () SupervisorState ()
onMessage message = do
    case message of
        Left (Dead cid reason) ->
            restartChild cid reason
        Right (Add cid spec) ->
            addChild cid spec
        Right Terminate -> stopProcess Shutdown


startup :: Process () SupervisorState ()
startup = do
    specs <- gets sChildSpec
    forM_ (M.assocs specs) $ \(id, spec) -> startChild id spec


terminate :: Reason -> Process () SupervisorState ()
terminate _reason = do
    threads <- gets sChildThread
    forM_ (M.keys threads) shutdownChild


startChild :: ChildId -> IO ChildSpec -> Process () SupervisorState ()
startChild cid spec' = do
    spec <- liftIO $ spec'
    stop <- liftIO $ newEmptyTMVarIO
    thId <- liftIO $ forkFinally (csAction spec) (shutdown stop)
    modify $ \s -> s { sChildThread = M.insert cid (thId, stop, spec) $ sChildThread s }
  where
    shutdown stop (Left e) = shutdown stop $ Right (Exception e)
    shutdown stop (Right reason) = atomically $ putTMVar stop reason


shutdownChild :: ChildId -> Process () SupervisorState ()
shutdownChild cid = do
    specs <- gets sChildSpec
    threads <- gets sChildThread
    let thread = M.lookup cid threads
    shutdownChild' thread
  where
    shutdownChild' (Just (thread, stop, spec)) = do
        timeouted <- liftIO $ T.timeout (csShutdownTimeout spec) (shutdownAttempt spec stop)
        when (isNothing timeouted) $
            liftIO $ killThread thread
        modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
    shutdownChild' _ = return ()

    shutdownAttempt spec stop = do
        liftIO $ csShutdown spec
        _ <- liftIO . atomically $ takeTMVar stop
        return ()


restartChild :: ChildId -> Reason -> Process () SupervisorState ()
restartChild cid reason = do
    crashes <- gets sCrashTime
    maxRestart <- gets sMaxRestart
    maxRestartTime <- gets sMaxRestartTime
    curtime <- liftIO getCurrentTime
    let crashes' = take (maxRestart + 1) (curtime : crashes)
    modify $ \s -> s { sCrashTime = crashes' }
    if needRestart maxRestart maxRestartTime crashes'
        then restartChild' cid reason
        else stopProcess reason


restartChild' :: ChildId -> Reason -> Process () SupervisorState ()
restartChild' cid reason = do
    specs <- gets sChildSpec
    workers <- gets sChildThread
    let spec = M.lookup cid specs
        worker = M.lookup cid workers
    case (spec, worker) of
        (Just spec1, Just (_, _, spec2)) -> do
            modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
            when (applyRestartPolicy (csRestart spec2) reason) $
                restartChild'' cid spec1 reason
        _ -> return ()


restartChild'' cid spec reason = do
    strategy <- gets sStrategy
    case strategy of
        OneForOne -> startChild cid spec
        OneForAll -> terminate reason >> startup


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

addChild :: ChildId -> IO ChildSpec -> Process () SupervisorState ()
addChild cid spec = do
    modify $ \s -> s { sChildSpec = M.insert cid spec (sChildSpec s) }
    startChild cid spec



