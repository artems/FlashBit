{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Server(..)
    , Reason(..)
    , Timeout
    , Response
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

type Response s = Either Reason s

data Server s m = Server
    { srvTimeout   :: Maybe Timeout
    , srvInit      :: s -> IO (Response s)
    , srvOnTimeout :: s -> IO (Response s)
    , srvOnMessage :: s -> m -> IO (Response s)
    , srvTerminate :: s -> Reason -> IO ()
    }

start :: s -> TChan m -> Server s m -> (Reason -> IO ()) -> IO (TMVar Reason)
start state chan server userTerminate = do
    stopT <- newEmptyTMVarIO
    _     <- forkFinally (startup stopT) shutdown
    return stopT
  where
    startup :: TMVar Reason -> IO Reason
    startup stopT = do
        stateT <- newTVarIO state
        reply  <- srvInit server state
        reason <- response stateT stopT chan server reply
                    `catch` (\(e :: SomeException) -> return (Exception e))
        state' <- atomically $ readTVar stateT
        srvTerminate server state' reason
        return reason

    shutdown :: Either SomeException Reason -> IO ()
    shutdown (Left e) = userTerminate (Exception e)
    shutdown (Right reason) = userTerminate reason


response :: TVar s -> TMVar Reason -> TChan m -> Server s m -> Response s -> IO Reason
response stateT stopT chan server reply
    = case reply of
        Left reason ->
            return reason
        Right state -> do
            updateState state
            loop stateT stopT chan server
  where
    updateState state = atomically $ writeTVar stateT state


loop :: TVar s -> TMVar Reason -> TChan m -> Server s m -> IO Reason
loop stateT stopT chan server = do
    state <- atomically $ readTVar stateT
    event <- case srvTimeout server of
                Just t  -> T.timeout t waitForEvent
                Nothing -> Just `fmap` waitForEvent
    reply <- onEvent state server event
    response stateT stopT chan server reply
  where
    waitForEvent = atomically $
        (readTChan chan >>= return . Right) `orElse`
        (takeTMVar stopT >>= return . Left)


onEvent :: s -> Server s m -> Maybe (Either Reason m) -> IO (Response s)
onEvent state server event
    = case event of
        Nothing -> srvOnTimeout server state
        Just (Left reason) -> return $ Left reason
        Just (Right message) -> srvOnMessage server state message


dummyServer :: Server s m
dummyServer = Server
    { srvInit = \state -> return $ Right state
    , srvTimeout  = Nothing
    , srvOnTimeout = \_state -> return $ Left Timeout
    , srvOnMessage = \state _message -> return $ Right state
    , srvTerminate = \_state _reason -> return ()
    }


