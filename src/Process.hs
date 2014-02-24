{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Process
    ( Process
    , runProcess
    , execProcess
    , stopProcess
    ) where


import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Data.Typeable


newtype Process conf state a = Process (ReaderT conf (StateT state IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader conf, MonadState state)

data StopProcessException = StopProcessException
  deriving (Show, Typeable)

instance Exception StopProcessException


runProcess :: conf -> state -> Process conf state a -> IO (a, state)
runProcess conf state (Process proc) = runStateT (runReaderT proc conf) state

execProcess :: conf -> state -> Process conf state a -> IO state
execProcess conf state proc = runProcess conf state proc >>= (\(_, s) -> return s)

stopProcess :: Process conf state a
stopProcess = liftIO . throwIO $ StopProcessException

