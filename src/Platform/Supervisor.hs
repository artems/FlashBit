module Platform.Supervisor
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
import Control.Monad.State hiding (state)

import qualified System.Timeout as T


import Platform.Server
import Platform.Process


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
    = AddChild ChildId ChildSpec
    | StopChild ChildId
    | DeleteChild ChildId
    | RestartChild ChildId
    | Terminate


data SupervisorState = SupervisorState
    { sStrategy         :: RestartStrategy
    , sMaxRestart       :: Int
    , sMaxRestartTime   :: Int
    , sCrashTime        :: [UTCTime]
    , sCommandChan      :: TChan SupervisorMessage
    , sChildOrder       :: [ChildId]
    , sChildStatic      :: IO [(ChildId, ChildSpec)]
    , sChildDynamic     :: [(ChildId, ChildSpec)]
    , sChildThread      :: M.Map ChildId (ThreadId, TMVar Reason, ChildSpec)
    }


mkSupervisor :: RestartStrategy -> Int -> Int
             -> TChan SupervisorMessage
             -> IO [(ChildId, ChildSpec)]
             -> IO SupervisorState
mkSupervisor strategy maxRestart maxRestartTime commandChan specs = do
    return $ SupervisorState
        { sStrategy = strategy
        , sMaxRestart = maxRestart
        , sMaxRestartTime = maxRestartTime
        , sCrashTime = []
        , sCommandChan = commandChan
        , sChildOrder = []
        , sChildStatic = specs
        , sChildDynamic = []
        , sChildThread = M.empty
        }

runSupervisor
    :: RestartStrategy -> Int -> Int
    -> Process () SupervisorState ()
    -> (Reason -> Process () SupervisorState ())
    -> TChan SupervisorMessage
    -> IO [(ChildId, ChildSpec)]
    -> IO Reason
runSupervisor strategy maxRestart maxRestartTime startup' terminate' chan specs  = do
    state <- mkSupervisor strategy maxRestart maxRestartTime chan specs
    runServer () state (startup' >> startup) server
  where
    server = mkServer wait onMessage (\reason -> terminate reason >> terminate' reason)


wait :: Process () SupervisorState (Either WorkerMessage SupervisorMessage)
wait = do
    threads <- gets sChildThread
    commandChan <- gets sCommandChan
    let command = readTChan commandChan >>= return . Right
        waitMessage = foldl myfold command (M.assocs threads)
    liftIO . atomically $ waitMessage
  where
    myfold stm (cid, (_, stop, _)) = stm `orElse`
        (readTMVar stop >>= return . Left . Dead cid)


onMessage :: Either WorkerMessage SupervisorMessage
          -> Process () SupervisorState ()
onMessage message = do
    case message of
        Left (Dead cid reason) ->
            restartChild cid reason
        Right (AddChild cid spec) ->
            addChild cid spec
        Right (StopChild _cid) ->
            undefined
        Right (DeleteChild _cid) ->
            undefined
        Right (RestartChild _cid) ->
            undefined
        Right Terminate ->
            stopProcess Shutdown


startup :: Process () SupervisorState ()
startup = do
    static <- liftIO `fmap` gets sChildStatic
    static' <- liftIO static
    dynamic <- gets sChildDynamic
    let children = if null dynamic then static' else static' ++ (reverse dynamic)
    forM_ children $ \(cid, spec)-> do
        startChild cid spec
    modify $ \s -> s { sChildOrder = reverse (map fst children) }


terminate :: Reason -> Process () SupervisorState ()
terminate _reason = do
    children <- gets sChildOrder
    forM_ children shutdownChild


startChild :: ChildId -> ChildSpec -> Process () SupervisorState ()
startChild cid spec = do
    stop <- liftIO $ newEmptyTMVarIO
    thId <- liftIO $ forkFinally (csAction spec) (shutdown stop)
    modify $ \s -> s { sChildThread = M.insert cid (thId, stop, spec) $ sChildThread s }
  where
    shutdown stop (Left e) = shutdown stop $ Right (Exception e)
    shutdown stop (Right reason) = atomically $ putTMVar stop reason


shutdownChild :: ChildId -> Process () SupervisorState ()
shutdownChild cid = do
    threads <- gets sChildThread
    let thread = M.lookup cid threads
    shutdownChild' thread
  where
    shutdownChild' :: Maybe (ThreadId, TMVar Reason, ChildSpec) -> Process () SupervisorState ()
    shutdownChild' (Just (thread, stop, spec)) = do
        timeouted <- liftIO $ T.timeout (csShutdownTimeout spec) (shutdownAttempt spec stop)
        when (isNothing timeouted) $ do
            liftIO $ killThread thread
            _ <- liftIO . atomically $ takeTMVar stop
            return ()
        modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
    shutdownChild' _ = return ()

    shutdownAttempt spec stop = do
        liftIO $ csShutdown spec
        liftIO . atomically $ takeTMVar stop


restartChild :: ChildId -> Reason -> Process () SupervisorState ()
restartChild cid reason = do
    crashes <- gets sCrashTime
    maxRestart <- gets sMaxRestart
    maxRestartTime <- gets sMaxRestartTime
    curtime <- liftIO getCurrentTime

    let crashes' = take (maxRestart + 1) (curtime : crashes)
    modify $ \s -> s { sCrashTime = crashes' }

    workers <- gets sChildThread
    let worker = M.lookup cid workers
    case worker of
        Just (_, _, spec) -> do
            modify $ \s -> s { sChildThread = M.delete cid $ sChildThread s }
            if needRestart maxRestart maxRestartTime crashes'
                then restartChild' cid spec reason
                else stopProcess reason
        Nothing -> return()


restartChild' :: ChildId -> ChildSpec -> Reason -> Process () SupervisorState ()
restartChild' cid spec reason = do
    when (applyRestartPolicy (csRestart spec) reason) $
        restartChild'' cid spec reason


restartChild'' :: ChildId -> ChildSpec -> Reason -> Process () SupervisorState ()
restartChild'' cid spec reason = do
    strategy <- gets sStrategy
    case strategy of
        OneForOne -> startChild cid spec
        OneForAll -> do
            terminate reason >> startup


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
    modify $ \s -> s
        { sChildOrder = cid : sChildOrder s
        , sChildDynamic = (cid, spec) : sChildDynamic s
        }
    startChild cid spec



