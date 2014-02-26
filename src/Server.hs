{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Server(..)
    , Reason(..)
    , Timeout
    , Response
    , start
    , simpleServer
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import qualified System.Timeout as T


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


type Timeout = Int

type Response s = Either Reason s

data Server s m = Server
    { srvTimeout   :: Maybe Timeout
    , srvInit      :: s -> IO (Response s)
    , srvOnTimeout :: s -> IO (Response s)
    , srvOnMessage :: s -> m -> IO (Response s)
    , srvTerminate :: s -> Reason -> IO Reason
    }

start :: s -> TChan m -> Server s m -> (Reason -> IO ()) -> IO (TMVar ())
start state chan server finally = do
    stopT <- newEmptyTMVarIO
    _ <- forkFinally (startup stopT) shutdown
    return stopT
  where
    startup :: TMVar () -> IO Reason
    startup stopT = mask $ \unmask -> do
        reply <- srvInit server state
        stateT <- newTVarIO state
        reason <- unmask $ response stateT stopT chan server reply
            `catch` (\(e :: SomeException) -> return (Exception e))
        state' <- atomically $ readTVar stateT
        srvTerminate server state' reason

    shutdown :: Either SomeException Reason -> IO ()
    shutdown (Left e) = finally (Exception e)
    shutdown (Right reason) = finally reason


response :: TVar s -> TMVar () -> TChan m -> Server s m -> Response s -> IO Reason
response stateT stopT chan server reply
    = case reply of
        Left reason -> return reason
        Right state -> do
            atomically $ writeTVar stateT state
            loop stateT stopT chan server


loop :: TVar s -> TMVar () -> TChan m -> Server s m -> IO Reason
loop stateT stopT chan server = do
    state <- atomically $ readTVar stateT
    event <- case srvTimeout server of
        Just t  -> T.timeout t readChan
        Nothing -> Just `fmap` readChan
    reply <- mask_ $ do
        onEvent state event
    response stateT stopT chan server reply
  where
    readChan = atomically $
        (readTChan chan >>= return . Right) `orElse`
        (takeTMVar stopT >>= return . Left)
    onEvent state Nothing = srvOnTimeout server state
    onEvent state (Just (Left _)) = return (Left Shutdown)
    onEvent state (Just (Right message)) = srvOnMessage server state message


simpleServer = Server
    { srvTimeout = Nothing
    , srvInit = (\s -> return (Right s))
    , srvOnTimeout = (\s -> return (Left Timeout))
    , srvOnMessage = (\s m -> return (Right s))
    , srvTerminate = (\s r -> return r)
    }


