{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Reason(..)
    , Timeout
    , mkServer
    , runServer
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified System.Timeout as T

import Process


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


data Server c s m = Server
    { srvWait       :: c -> s -> STM m
    , srvTimeout    :: Maybe Timeout
    , srvOnTimeout  :: Process c s (Maybe Reason)
    , srvOnMessage  :: m -> Process c s (Maybe Reason)
    , srvTerminate  :: Reason -> Process c s ()
    }


mkServer :: (c -> s -> STM m)
         -> Maybe Timeout
         -> Process c s (Maybe Reason)
         -> (m -> Process c s (Maybe Reason))
         -> (Reason -> Process c s ())
         -> Server c s m
mkServer wait timeout onTimeout onMessage terminate
    = Server
    { srvWait = wait
    , srvTimeout = timeout
    , srvOnTimeout = onTimeout
    , srvOnMessage = onMessage
    , srvTerminate = terminate
    }


runServer :: c -> s -> Process c s (Maybe Reason) -> Server c s m -> IO Reason
runServer conf state init server = do
    (reason, state') <- runProcess conf state init
    startup conf state' server reason


startup :: c -> s -> Server c s m -> Maybe Reason -> IO Reason
startup _conf _state _server (Just reason) =
    return reason

startup conf state server Nothing = do
    stateT <- newTVarIO state
    reason <- loop conf stateT server
        `catch` (\(e :: SomeException) -> return (Exception e))
    state' <- atomically $ readTVar stateT
    runProcess conf state' $ srvTerminate server reason
    return reason


loop :: c -> TVar s -> Server c s m -> IO Reason
loop conf stateT server = do
    state <- atomically $ readTVar stateT
    event <- case srvTimeout server of
        Just x  -> T.timeout x (waitForEvent conf state)
        Nothing -> Just `fmap` (waitForEvent conf state)
    onEvent conf stateT server event
  where
    waitForEvent conf state = atomically $ srvWait server conf state


onEvent :: c -> TVar s -> Server c s m -> Maybe m -> IO Reason
onEvent conf stateT server event = do
    state <- atomically $ readTVar stateT
    (reason, state') <- runProcess conf state (proc event)
    atomically $ writeTVar stateT state'
    response reason
  where
    proc Nothing = srvOnTimeout server
    proc (Just message) = srvOnMessage server message

    response Nothing = loop conf stateT server
    response (Just reason) = return reason


