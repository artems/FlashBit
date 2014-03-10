{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Process
    ( specWorker
    , specServer
    , specServer2
    , specSupervisor
    , process0
    , process1
    , supervisor0
    , noStartup
    , noTerminate

    , ProcessName(..)
    , logP, infoP, debugP
    , errorP, warningP, criticalP
    ) where

import Control.Concurrent.STM
import Control.Exception hiding (Handler)

import System.Log.Logger

import Control.Monad.Reader (liftIO, asks)

import Platform.Process
import Platform.Server
import Platform.Supervisor


specWorker :: IO () -> IO ChildSpec
specWorker action = return $
    ChildSpec
        { csType = Worker
        , csAction = action >> return Normal
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 0
        }

specServer :: IO Reason -> IO ChildSpec
specServer action = return $
    ChildSpec
        { csType = Worker
        , csAction = action
        , csRestart = Permanent
        , csShutdown = return ()
        , csShutdownTimeout = 0
        }

specServer2 :: TChan m -> m -> IO Reason -> IO ChildSpec
specServer2 chan m action = return $
    ChildSpec
        { csType = Worker
        , csAction = action
        , csRestart = Permanent
        , csShutdown = atomically $ writeTChan chan m
        , csShutdownTimeout = 5 * 1000000
        }


specSupervisor :: IO Reason -> TChan SupervisorMessage -> IO ChildSpec
specSupervisor action superChan = do
    return $ ChildSpec
        { csType = Supervisor
        , csAction = action
        , csRestart = Permanent
        , csShutdown = atomically $ writeTChan superChan Terminate
        , csShutdownTimeout = 10 * 1000000 -- 10 sec
        }


process0 :: String -> conf -> state
        -> Process conf state m
        -> (m -> Process conf state ())
        -> IO Reason
process0 name conf state wait receive =
    runServer conf state startup server
  where
    server = mkServer wait receive terminate
    startup = liftIO $ debugM name "Процесс запущен"
    terminate reason = liftIO $ debugM name $ printReason reason


process1 :: String -> conf -> state
        -> Process conf state m
        -> (m -> Process conf state ())
        -> (Reason -> Process conf state ())
        -> IO Reason
process1 name conf state wait receive terminate =
    runServer conf state startup server
  where
    server = mkServer wait receive (\reason -> terminate reason >> terminate' reason)
    startup = liftIO $ debugM name "Процесс запущен"
    terminate' reason = liftIO $ debugM name $ printReason reason


supervisor0
    :: String
    -> TChan SupervisorMessage
    -> [(ChildId, IO ChildSpec)]
    -> IO Reason
supervisor0 name superChan specs =
    runSupervisor' OneForOne 3 60 startup terminate superChan specs
  where
    startup = liftIO $ debugM name "Процесс запущен"
    terminate reason = liftIO $ debugM name $ printReason reason


noStartup :: Process a b ()
noStartup = return ()

noTerminate :: Reason -> Process a b ()
noTerminate _reason = return ()


data Handler a = forall e . Exception e => Handler (e -> String)

printReason :: Reason -> String
printReason reason = case reason of
    Normal ->
        "Процесс остановился сам"
    Timeout ->
        "Процесс остановлен по таймауту"
    Shutdown ->
        "Процесс остановлен по команде"
    UserReason r ->
        "Процесс остановлен по причине: " ++ r
    Exception e -> foldr tryHandler defaultHandler handlers
      where
        handlers =
            [ Handler (\ThreadKilled -> "остановлен супервайзером")
            ]
        defaultHandler = "В процессе не обработано исключение: " ++ show e
        tryHandler (Handler handler) res =
            case (fromException . toException) e of
                Just e' -> handler e'
                Nothing -> res


class ProcessName conf where
    processName :: conf -> String


logP :: (ProcessName conf) => Priority -> String -> Process conf state ()
logP priority message = do
    name <- asks processName
    liftIO $ logM name priority message

infoP :: (ProcessName conf) => String -> Process conf state ()
infoP = logP INFO

debugP :: (ProcessName conf) => String -> Process conf state ()
debugP = logP DEBUG

errorP :: (ProcessName conf) => String -> Process conf state ()
errorP = logP ERROR

warningP :: (ProcessName conf) => String -> Process conf state ()
warningP = logP WARNING

criticalP :: (ProcessName conf) => String -> Process conf state ()
criticalP = logP CRITICAL

