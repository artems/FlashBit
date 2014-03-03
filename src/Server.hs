{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Server(..)
    , Reason(..)
    , Timeout
    , dummyServer
    , start
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified System.Timeout as T


type Timeout = Int

data Reason
    = Normal
    | Timeout
    | Shutdown
    | Exception SomeException
    | UserReason String
    deriving (Show)


instance Eq Reason where
    Normal == Normal = True
    Timeout == Timeout = True
    Shutdown == Shutdown = True
    UserReason a == UserReason b = a == b
    _ == _ = False


data Server s m = Server
    { srvInit      :: IO (Either Reason s)
    , srvChan      :: TChan m
    , srvTimeout   :: Maybe Timeout
    , srvOnTimeout :: s -> IO (Either Reason s)
    , srvTerminate :: s -> Reason -> IO ()
    , srvOnMessage :: s -> m -> IO (Either Reason s)
    }


start :: Server s m -> (Reason -> IO ()) -> IO (TMVar Reason)
start server userTerminate = do
    stopT <- newEmptyTMVarIO
    _     <- forkFinally (startup server stopT) (shutdown userTerminate)
    return stopT


startup :: Server s m -> TMVar Reason -> IO Reason
startup server stopT = do
    reply <- srvInit server
    startup' server stopT reply


startup' :: Server s m -> TMVar Reason -> Either Reason s -> IO Reason
startup' _server _stopT (Left reason) =
    return reason

startup' server stopT (Right state) = do
    stateT <- newTVarIO state
    reason <- loop server stateT stopT
        `catch` (\(e :: SomeException) -> return (Exception e))
    state' <- atomically $ readTVar stateT
    srvTerminate server state' reason
    return reason


shutdown :: (Reason -> IO ()) -> Either SomeException Reason -> IO ()
shutdown userTerminate (Left e)
    = userTerminate $ Exception e
shutdown userTerminate (Right reason)
    = userTerminate reason


loop :: Server s m -> TVar s -> TMVar Reason -> IO Reason
loop server stateT stopT = do
    state <- atomically $ readTVar stateT
    event <- case srvTimeout server of
        Just x  -> T.timeout x waitForEvent
        Nothing -> Just `fmap` waitForEvent
    reply <- onEvent server state event
    response server stateT stopT reply
  where
    chan = srvChan server
    waitForEvent = atomically $
        (readTChan chan >>= return . Right) `orElse`
        (takeTMVar stopT >>= return . Left)


onEvent :: Server s m -> s -> Maybe (Either Reason m) -> IO (Either Reason s)
onEvent server state event
    = case event of
        Nothing -> srvOnTimeout server state
        Just (Left reason) -> return $ Left reason
        Just (Right message) -> srvOnMessage server state message


response :: Server s m -> TVar s -> TMVar Reason -> Either Reason s -> IO Reason
response server stateT stopT reply = case reply of
    Left reason ->
        return reason
    Right state -> do
        atomically $ writeTVar stateT state
        loop server stateT stopT


dummyServer :: Server s m
dummyServer = Server
    { srvInit = undefined
    , srvChan = undefined
    , srvTimeout  = Nothing
    , srvOnTimeout = \_state -> return $ Left Timeout
    , srvTerminate = \_state _reason -> return ()
    , srvOnMessage = undefined
    }


