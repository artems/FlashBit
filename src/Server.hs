{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( mkServer
    , mkServerWithTimeout
    , runServer
    ) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified System.Timeout as T

import Process


data Server c s m = Server
    { srvWait       :: Process c s m
    , srvTimeout    :: Maybe Int
    , srvOnTimeout  :: Process c s ()
    , srvOnMessage  :: m -> Process c s ()
    , srvTerminate  :: Reason -> Process c s ()
    }


mkServer :: Process c s m
         -> (m -> Process c s ())
         -> (Reason -> Process c s ())
         -> Server c s m
mkServer wait onMessage terminate
    = Server
    { srvWait = wait
    , srvTimeout = Nothing
    , srvOnTimeout = stopProcess Timeout
    , srvOnMessage = onMessage
    , srvTerminate = terminate
    }

mkServerWithTimeout
    :: Process c s m
    -> (m -> Process c s ())
    -> (Reason -> Process c s ())
    -> Int
    -> Process c s ()
    -> Server c s m
mkServerWithTimeout wait onMessage terminate timeout onTimeout =
    let server = mkServer wait onMessage terminate
    in  server
        { srvTimeout = Just timeout
        , srvOnTimeout = onTimeout
        }


runP conf state proc = do
    result <- (execProcess conf state proc >>= return . Right)
        `catches`
            [ Handler (\(StopProcessException reason) -> return . Left $ reason)
            , Handler (\(e :: SomeException) -> return . Left $ Exception e)
            ]
    return result


runServer :: c -> s -> Process c s () -> Server c s m -> IO Reason
runServer conf state init server = do
    result <- runP conf state init
    case result of
        Left reason -> return reason
        Right state' -> startup conf state' server


startup :: c -> s -> Server c s m -> IO Reason
startup conf state server = do
    stateT <- newTVarIO state
    reason <- loop conf state server stateT
        `catch` (\(e :: SomeException) -> return (Exception e))
    state' <- atomically $ readTVar stateT
    -- NOTE: ignore exeption raised from terminate process
    _ <- runP conf state' $ srvTerminate server reason
    return reason


loop :: c -> s -> Server c s m -> TVar s -> IO Reason
loop conf state server stateT = do
    event <- case srvTimeout server of
        Just x  -> T.timeout x waitForEvent
        Nothing -> Just `fmap` waitForEvent
    onEvent conf state server stateT event
  where
    -- NOTE: ignore state changes from wait process
    waitForEvent = evalProcess conf state $ srvWait server


onEvent :: c -> s -> Server c s m -> TVar s -> Maybe m -> IO Reason
onEvent conf state server stateT event = do
    result <- runP conf state (proc event)
    case result of
        Left reason ->
            return reason
        Right state' -> do
            atomically $ writeTVar stateT state'
            loop conf state' server stateT
  where
    proc Nothing = srvOnTimeout server
    proc (Just message) = srvOnMessage server message


