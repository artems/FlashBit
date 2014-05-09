module ProcessGroup
    ( ProcessGroup
    , runGroup
    , initGroup
    , stopGroup
    ) where

import Control.Concurrent
import Control.Exception


data StopReason
    = Terminate
    | ChildDead (Either SomeException ())

type ProcessGroup = MVar StopReason


runAction :: ProcessGroup -> IO () -> IO ThreadId
runAction stopM action =
    forkFinally action (stopAction stopM)


stopAction :: ProcessGroup -> Either SomeException () -> IO ()
stopAction stopM exception =
    putMVar stopM (ChildDead exception)


initGroup :: IO (ProcessGroup)
initGroup = newEmptyMVar


stopGroup :: ProcessGroup -> IO ()
stopGroup stopM =
    tryPutMVar stopM Terminate >> return ()


forkGroup :: ProcessGroup -> [IO ()] -> IO [ThreadId]
forkGroup stopM = mapM (runAction stopM)


waitAny :: ProcessGroup -> [ThreadId] -> IO (Either SomeException ())
waitAny stopM _threads = do
    reason <- readMVar stopM
    case reason of
        Terminate   -> return $ Right ()
        ChildDead e -> return $ e


waitAll :: ProcessGroup -> Int -> IO ()
waitAll _     0     = return ()
waitAll stopM count = do
    reason <- takeMVar stopM
    case reason of
        Terminate   -> waitAll stopM count
        ChildDead _ -> waitAll stopM (count - 1)


shutdownGroup :: ProcessGroup -> Int -> [ThreadId] -> IO ()
shutdownGroup stopM count threads = do
    mapM_ killThread threads
    waitAll stopM count


runGroup :: ProcessGroup -> [IO ()] -> IO (Either SomeException ())
runGroup stopM []    = return . Right $ ()
runGroup stopM group = do
    let count = length group
    bracket (forkGroup stopM group)
            (shutdownGroup stopM count)
            (waitAny stopM)
        `catch`
            (return . Left)


