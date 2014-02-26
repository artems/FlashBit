module Supervisor
    ( start
    , RestartPolicy(..)
    , RestartStrategy(..)
    , ChildId
    , ChildType(..)
    , ChildSpec(..)
    , SupervisorMessage(..)
    , SupervisorChannel
    ) where


import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State hiding (state)

import Data.Time.Clock

import Server (Server(..), Reason(..), simpleServer)
import qualified Server as Server


type MaxTime = Int

type MaxRestart = Int

type ShutdownTimeout = Int

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
    , csAction :: (Reason -> IO ()) -> IO (TMVar ())
    , csRestart :: RestartPolicy
    , csShutdown :: ShutdownTimeout
    }

data SupervisorMessage
    = AddChild ChildId ChildSpec
    | ChildDead ChildId Reason
    | Terminate

type SupervisorChannel = TChan SupervisorMessage

instance Show SupervisorMessage where
    show (AddChild id _)  = "add_child#" ++ show id
    show (ChildDead id _) = "child_dead#" ++ show id
    show Terminate        = "terminate"


data SupervisorState = SupervisorState
    { sChan :: SupervisorChannel
    , sStrategy :: RestartStrategy
    , sMaxRestart :: MaxRestart
    , sMaxTime :: MaxTime
    , sRestartMark :: [UTCTime]
    , sChildrenSpec :: M.Map ChildId ChildSpec
    , sChildrenStop :: M.Map ChildId (TMVar ())
    , sReason :: Maybe Reason
    }

type Supervisor = StateT SupervisorState IO ()


supervisorServer = simpleServer
    { srvInit = onInit
    , srvOnMessage = onMessage
    , srvTerminate = onTerminate
    }

runSupervisor state supervisor = do
    state' <- execStateT supervisor state
    case (sReason state') of
        Just reason -> return (Left reason)
        Nothing     -> return (Right state')

onInit state = runSupervisor state startup

onMessage state Terminate
    = runSupervisor state stop
onMessage state (AddChild id spec)
    = runSupervisor state (addChild id spec)
onMessage state (ChildDead id reason)
    = runSupervisor state (reanimateChild id reason)

onTerminate state reason
    = execStateT terminate state >> return reason


start :: RestartStrategy -> MaxRestart -> MaxTime -> [(ChildId, ChildSpec)]
      -> (Reason -> IO ())
      -> IO (TMVar (), SupervisorChannel)
start strategy maxRestart maxTime specs finally = do
    chan <- newTChanIO
    state <- mkSupervisorState chan strategy maxRestart maxTime specs
    stopT <- Server.start state chan supervisorServer finally
    return (stopT, chan)


stop :: Supervisor
stop = modify $ \state -> state { sReason = Just Shutdown }


startup :: Supervisor
startup = do
    children <- gets sChildrenSpec
    forM_ (M.assocs children) $ \(id, spec) -> startChild id spec


terminate :: Supervisor
terminate = do
    children <- gets sChildrenStop
    forM_ (M.elems children) $ \stopT -> liftIO . atomically $ putTMVar stopT ()
    -- TODO shutdown timeout
    -- TODO wait for children


startChild :: ChildId -> ChildSpec -> Supervisor
startChild id spec = do
    chan <- gets sChan
    stopT <- liftIO $ csAction spec (finally chan)
    modify $ \state -> state {
            sChildrenStop = M.insert id stopT (sChildrenStop state)
        }
  where
    finally chan reason = atomically $ writeTChan chan (ChildDead id reason)



mkSupervisorState chan strategy maxRestart maxTime specs = do
    return SupervisorState
        { sChan = chan
        , sStrategy = strategy
        , sMaxRestart = maxRestart
        , sMaxTime = maxTime
        , sRestartMark = []
        , sChildrenSpec = M.fromList specs
        , sChildrenStop = M.empty
        , sReason = Nothing
        }


addChild :: ChildId -> ChildSpec -> Supervisor
addChild id spec = do
    modify $ \state -> state {
            sChildrenSpec = M.insert id spec (sChildrenSpec state)
        }
    startChild id spec

deleteChild :: ChildId -> Supervisor
deleteChild id = do
    modify $ \state -> state {
            sChildrenSpec = M.delete id (sChildrenSpec state),
            sChildrenStop = M.delete id (sChildrenStop state)
        }

reanimateChild :: ChildId -> Reason -> Supervisor
reanimateChild id reason = do
    crashes <- gets sRestartMark
    maxTime <- gets sMaxTime
    maxRestart <- gets sMaxRestart
    curtime <- liftIO getCurrentTime
    let crashes' = take maxRestart $ curtime : crashes
    modify $ \state -> state { sRestartMark = crashes' }
    if checkRestart maxRestart maxTime crashes
        then restartChild id reason
        else stop


checkRestart maxRestart maxTime crashes
    | length crashes < maxRestart = False
    | otherwise = let
        hi = head crashes
        lo = last crashes
        in floor (diffUTCTime hi lo) > maxTime

restartChild :: ChildId -> Reason -> Supervisor
restartChild id reason = do
    strategy <- gets sStrategy
    children <- gets sChildrenSpec
    let spec = findSpec id children
    if needRestart spec reason
        then restart strategy spec
        else deleteChild id

  where
    findSpec id children =
        case M.lookup id children of
            Just spec -> spec
            Nothing   -> error "impossible"

    restart strategy spec
        = case strategy of
            OneForOne -> startChild id spec
            OneForAll -> terminate >> startup

    needRestart spec reason
        = case csRestart spec of
            Permanent -> True
            Temporary -> False
            Transient -> reason /= Normal


