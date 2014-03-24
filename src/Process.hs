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
    , stopProcess
    , wrapProcess
    , catchProcess
    , logP
    , infoP
    , debugP
    , errorP
    , warningP
    , criticalP
    ) where

import Control.Concurrent

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Typeable

import System.Log.Logger


newtype Process conf state a = Process (ReaderT conf (StateT state IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader conf, MonadState state)


data StopProcessException = StopProcessException
  deriving (Show, Typeable)


instance Exception StopProcessException


runProcess :: pconf -> pstate -> Process pconf pstate a -> IO (a, pstate)
runProcess pconf pstate (Process proc) = runStateT (runReaderT proc pconf) pstate

execProcess :: conf -> state -> Process conf state a -> IO state
execProcess pconf pstate proc = runProcess pconf pstate proc >>= (\(_, s) -> return s)

evalProcess :: conf -> state -> Process conf state a -> IO a
evalProcess pconf pstate proc = runProcess pconf pstate proc >>= (\(a, _) -> return a)

stopProcess :: Process pconf pstate a
stopProcess = liftIO . throwIO $ StopProcessException

wrapProcess :: (ProcessName pconf) => pconf -> pstate -> Process pconf pstate a -> IO ()
wrapProcess pconf pstate proc = do
    catchProcess pconf pstate proc (\_ -> return ())

catchProcess :: (ProcessName pconf)
    => pconf -> pstate
    -> Process pconf pstate a
    -> (pconf -> IO ())
    -> IO ()
catchProcess pconf pstate proc terminate = do
    let name = processName pconf
    bracket_
        (debugM name "Старт")
        (debugM name "Выход")
        (catches (action `onException` terminate pconf)
            [ Handler  (\ThreadKilled -> debugM name $ "Остановлен")
            , Handler  (\StopProcessException -> debugM name $ "Завершение")
            , Handler  (\(e :: SomeException) -> errorM name $ "Не обработано исключение: " ++ show e)
            ]
        )
  where
    action = runProcess pconf pstate proc >> return ()



class ProcessName pconf where
    processName :: pconf -> String

logP :: (ProcessName pconf) => Priority -> String -> Process pconf pstate ()
logP priority message = do
    name <- asks processName
    liftIO $ logM name priority message

infoP :: (ProcessName pconf) => String -> Process pconf pstate ()
infoP = logP INFO

debugP :: (ProcessName pconf) => String -> Process pconf pstate ()
debugP = logP DEBUG

errorP :: (ProcessName pconf) => String -> Process pconf pstate ()
errorP = logP ERROR

warningP :: (ProcessName pconf) => String -> Process pconf pstate ()
warningP = logP WARNING

criticalP :: (ProcessName pconf) => String -> Process pconf pstate ()
criticalP = logP CRITICAL


