{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Process
    ( Process
    , runProcess
    , execProcess
    , evalProcess
    ) where


import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Data.Typeable


newtype Process conf state a = Process (ReaderT conf (StateT state IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader conf, MonadState state)


runProcess :: conf -> state -> Process conf state a -> IO (a, state)
runProcess conf state (Process proc) = runStateT (runReaderT proc conf) state

execProcess :: conf -> state -> Process conf state a -> IO state
execProcess conf state proc = runProcess conf state proc >>= (\(_, s) -> return s)

evalProcess :: conf -> state -> Process conf state a -> IO a
evalProcess conf state proc = runProcess conf state proc >>= (\(a, _) -> return a)


