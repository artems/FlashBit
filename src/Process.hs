{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Process
    ( Process
    , Reason(..)
    , StopProcessException(..)
    , runProcess
    , execProcess
    , evalProcess
    , stopProcess
    ) where


import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Data.Typeable


newtype Process conf state a = Process (ReaderT conf (StateT state IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader conf, MonadState state)


data Reason
    = Normal
    | Timeout
    | Shutdown
    | forall e. Exception e => Exception e
    | UserReason String

instance Eq Reason where
    Normal == Normal = True
    Timeout == Timeout = True
    Shutdown == Shutdown = True
    UserReason a == UserReason b = a == b
    _ == _ = False

instance Show Reason where
    show Normal = "Normal"
    show Timeout = "Timeout"
    show Shutdown = "Shutdown"
    show (Exception e) = "Exception: " ++ show e
    show (UserReason a) = "UserReason: " ++ show a


data StopProcessException = StopProcessException Reason
  deriving (Show, Typeable)

instance Exception StopProcessException


runProcess :: conf -> state -> Process conf state a -> IO (a, state)
runProcess conf state (Process proc) = runStateT (runReaderT proc conf) state

execProcess :: conf -> state -> Process conf state a -> IO state
execProcess conf state proc = runProcess conf state proc >>= (\(_, s) -> return s)

evalProcess :: conf -> state -> Process conf state a -> IO a
evalProcess conf state proc = runProcess conf state proc >>= (\(a, _) -> return a)

stopProcess :: Reason -> Process conf state a
stopProcess reason = liftIO . throwIO $ StopProcessException reason


