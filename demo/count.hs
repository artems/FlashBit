{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forever)
import Control.Monad.State
import System.Random (randomRIO)

import Server
import Process


data MyMessage = Add | Reset | Print | Wait (TMVar ())

wait conf state = readTChan conf

onMessage m = do
    case m of
        Add ->
            modify (+1)
        Reset ->
            put 0
        Print -> do
            count <- get
            liftIO . putStrLn $ "count " ++ show count
        Wait v ->
            liftIO . atomically $ putTMVar v ()
    return Nothing

terminate reason = do
    count <- get
    liftIO . putStrLn $ "count " ++ show count ++ "; reason " ++ show reason

server = mkServer wait Nothing (return (Just Timeout)) onMessage terminate

main = do
    chan <- newTChanIO
    stop <- newEmptyTMVarIO
    let sinit = return Nothing
    thId <- forkFinally
        (runServer chan 0 sinit server)
        (\reason -> atomically $ putTMVar stop reason)
    putStrLn $ "thread_id: " ++ show thId

    forkIO $ proc chan
    reason <- catch
        (atomically $ takeTMVar stop)
        (\e@UserInterrupt -> do
            throwTo thId e
            atomically $ takeTMVar stop
        )
    return ()
  where
    proc chan = do
        v <- newEmptyTMVarIO
        num <- randomRIO ((5000, 8000) :: (Int, Int))
        putStrLn $ "\nnum = " ++ show num
        forM [1..num] $ \i -> do
            atomically $ writeTChan chan Add
        atomically $ do
            writeTChan chan Print
            writeTChan chan Reset
            writeTChan chan (Wait v)
        atomically $ takeTMVar v
        proc chan


