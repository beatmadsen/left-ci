{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.DataStore.Atomic
  ( AtomicM(..)
  , executeAtomic
  ) where

import Control.Monad.Reader ( MonadIO, MonadReader, ReaderT(..), runReaderT )


-- | Represents operations that should be performed atomically.
-- The context type 'ctx' is determined by the specific implementation
-- (could be a Connection, an IORef, etc.)
newtype AtomicM ctx a = AtomicM 
  { runAtomicM :: ReaderT ctx IO a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader ctx  -- This lets implementations access their context
    ) 

executeAtomic :: AtomicM ctx a -> ctx -> IO a
executeAtomic atomicAction context = runReaderT (runAtomicM atomicAction) context
