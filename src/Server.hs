{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Reason(..)
    , Timeout
    , mkServer
    , mkServerWithTimeout
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
    { srvWait       :: Process c s m
    , srvTimeout    :: Maybe Timeout
    , srvOnTimeout  :: Process c s (Maybe Reason)
    , srvOnMessage  :: m -> Process c s (Maybe Reason)
    , srvTerminate  :: Reason -> Process c s ()
    }


mkServer :: Process c s m
         -> (m -> Process c s (Maybe Reason))
         -> (Reason -> Process c s ())
         -> Server c s m
mkServer wait onMessage terminate
    = Server
    { srvWait = wait
    , srvTimeout = Nothing
    , srvOnTimeout = return $ Just Timeout
    , srvOnMessage = onMessage
    , srvTerminate = terminate
    }

mkServerWithTimeout
    :: Process c s m
    -> (m -> Process c s (Maybe Reason))
    -> (Reason -> Process c s ())
    -> Maybe Timeout
    -> Process c s (Maybe Reason)
    -> Server c s m
mkServerWithTimeout wait onMessage terminate timeout onTimeout =
    let server = mkServer wait onMessage terminate
    in  server
        { srvTimeout = timeout
        , srvOnTimeout = onTimeout
        }


runServer :: c -> s -> Server c s m -> IO Reason
runServer conf state server = do
    stateT <- newTVarIO state
    reason <- loop conf state server stateT
        `catch` (\(e :: SomeException) -> return (Exception e))
    state' <- atomically $ readTVar stateT
    runProcess conf state' $ srvTerminate server reason
    return reason


loop :: c -> s -> Server c s m -> TVar s -> IO Reason
loop conf state server stateT = do
    event <- case srvTimeout server of
        Just x  -> T.timeout x waitForEvent
        Nothing -> Just `fmap` waitForEvent
    onEvent conf state server stateT event
  where
    -- NOTE: state changes ignored
    waitForEvent = evalProcess conf state $ srvWait server


onEvent :: c -> s -> Server c s m -> TVar s -> Maybe m -> IO Reason
onEvent conf state server stateT event = do
    (reason, state') <- runProcess conf state (proc event)
    case reason of
        Just reason ->
            return reason
        Nothing -> do
            atomically $ writeTVar stateT state'
            loop conf state' server stateT
  where
    proc Nothing = srvOnTimeout server
    proc (Just message) = srvOnMessage server message


