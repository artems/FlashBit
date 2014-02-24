module Supervisor
    ( start
    ) where


import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State hiding (state)

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
    , sChildrenSpec :: M.Map ChildId ChildSpec
    , sChildrenStop :: M.Map ChildId (TMVar ())
    }

type Supervisor = StateT SupervisorState IO ()


supervisorServer = simpleServer
    { srvInit = onInit
    , srvOnMessage = onMessage
    , srvTerminate = onTerminate
    }

runSupervisor state supervisor
    = Right `liftM` execStateT supervisor state

onInit state = runSupervisor state startup

onMessage state Terminate
    = return $ Left Shutdown
onMessage state (AddChild id spec)
    = runSupervisor state (addChild id spec)
onMessage state (ChildDead id reason)
    = runSupervisor state (reanimateChild id reason)

onTerminate state reason
    = execStateT terminate state >> return reason


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


start :: RestartStrategy -> [(ChildId, ChildSpec)]
      -> IO (TMVar (), SupervisorChannel)
start strategy specs = do
    chan <- newTChanIO
    state <- mkSupervisorState chan strategy specs
    stopT <- Server.start state chan supervisorServer finally
    return (stopT, chan)
  where
    finally reason = return ()


mkSupervisorState chan strategy specs = do
    return SupervisorState
        { sChan = chan
        , sStrategy = strategy
        , sChildrenSpec = M.fromList specs
        , sChildrenStop = M.empty
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


