{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Process
    ( Process
    , ProcessName(..)
    , StopProcessException(..)
    , runProcess
    , execProcess
    , evalProcess
    , wrapProcess
    , stopProcess
    ) where

import Control.Concurrent

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Data.Typeable

import System.Log.Logger


newtype Process conf state a = Process (ReaderT conf (StateT state IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader conf, MonadState state)


data StopProcessException = StopProcessException
  deriving (Show, Typeable)


instance Exception StopProcessException


runProcess :: conf -> state -> Process conf state a -> IO (a, state)
runProcess conf state (Process proc) = runStateT (runReaderT proc conf) state

execProcess :: conf -> state -> Process conf state a -> IO state
execProcess conf state proc = runProcess conf state proc >>= (\(_, s) -> return s)

evalProcess :: conf -> state -> Process conf state a -> IO a
evalProcess conf state proc = runProcess conf state proc >>= (\(a, _) -> return a)

stopProcess :: Process conf state a
stopProcess = liftIO . throwIO $ StopProcessException

wrapProcess :: (ProcessName conf) => conf -> state -> Process conf state a -> IO ()
wrapProcess conf state proc = do
    let name = processName conf
    bracket_
        (debugM name "Старт")
        (debugM name "Выход")
        (catches action
            [ Handler  (\ThreadKilled -> debugM name $ "Остановлен")
            , Handler  (\StopProcessException -> debugM name $ "Завершение")
            , Handler  (\(e :: SomeException) -> debugM name $ "Не обработано исключение: " ++ show e)
            ]
        )
  where
    action = runProcess conf state proc >> return ()


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


