module ProcessGroup
    ( bracketGroup
    ) where

import Control.Concurrent
import Control.Exception


runAction :: MVar (Either SomeException ()) -> IO () -> IO ThreadId
runAction stopM action = forkFinally action (stopAction stopM)

stopAction :: MVar (Either SomeException ()) -> Either SomeException () -> IO ()
stopAction stopM exception = putMVar stopM exception

forkGroup :: MVar (Either SomeException ()) -> [IO ()] -> IO [ThreadId]
forkGroup stopM = mapM (runAction stopM)

waitAny :: MVar (Either SomeException ()) -> [ThreadId] -> IO (Either SomeException ())
waitAny stopM _threadIds = takeMVar stopM

waitAll :: MVar (Either SomeException ()) -> Int -> IO ()
waitAll _     0     = return ()
waitAll stopM count = do
    _ <- takeMVar stopM
    waitAll stopM (count - 1)

shutdownGroup :: [ThreadId] -> IO ()
shutdownGroup = mapM_ killThread

bracketGroup :: [IO ()] -> IO (Either SomeException ())
bracketGroup []    = return . Right $ ()
bracketGroup group = do
    stopM  <- newEmptyMVar
    result <- bracket
            (forkGroup stopM group)
            (shutdownGroup)
            (waitAny stopM)
        `catch`
            (return . Left)
    waitAll stopM (length group - 1)
    return result


