{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | MTL-style typeclass for IR builder operations.

This module provides the 'MonadIRBuilder' typeclass, which abstracts over
the core operations needed for LLVM IR construction. It follows the MTL pattern
of providing lift-through instances for common transformers, allowing
'IRBuilderT' to be stacked with 'ReaderT', 'StateT', 'WriterT', etc.

The class provides three primitives:

- 'getIRBuilderEnv': Access the current builder state
- 'putIRBuilderEnv': Replace the builder state
- 'throwIRBuilderError': Throw a builder error

Default methods 'modifyIRBuilderEnv' and 'getsIRBuilderEnv' are provided
for convenience.
-}
module LLVM.IRBuilder.Class (
  MonadIRBuilder (..),
)
where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState (get, put), StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import LLVM.IRBuilder.Environment (IRBuilderEnv)
import LLVM.IRBuilder.Error (IRBuilderError)

{- | MTL-style typeclass for monads that support IR building operations.

This class provides primitives for accessing and modifying the builder
environment and throwing builder-specific errors. It is designed to be
composable with other monad transformers via lift-through instances.

Minimal complete definition: 'getIRBuilderEnv', 'putIRBuilderEnv', 'throwIRBuilderError'
-}
class (Monad m) => MonadIRBuilder m where
  -- | Retrieve the current builder environment.
  getIRBuilderEnv :: m IRBuilderEnv

  -- | Replace the current builder environment.
  putIRBuilderEnv :: IRBuilderEnv -> m ()

  -- | Throw a builder error, short-circuiting the computation.
  throwIRBuilderError :: IRBuilderError -> m a

  -- | Modify the builder environment using a function.
  --
  -- Default implementation in terms of 'getIRBuilderEnv' and 'putIRBuilderEnv'.
  modifyIRBuilderEnv :: (IRBuilderEnv -> IRBuilderEnv) -> m ()
  modifyIRBuilderEnv f = do
    env <- getIRBuilderEnv
    putIRBuilderEnv (f env)

  -- | Retrieve a projection of the builder environment.
  --
  -- Default implementation in terms of 'getIRBuilderEnv'.
  getsIRBuilderEnv :: (IRBuilderEnv -> a) -> m a
  getsIRBuilderEnv f = f <$> getIRBuilderEnv

-- Lift-through instances for common transformers

-- | Lift through 'StateT'.
instance (MonadIRBuilder m) => MonadIRBuilder (StateT s m) where
  getIRBuilderEnv = lift getIRBuilderEnv
  putIRBuilderEnv = lift . putIRBuilderEnv
  throwIRBuilderError = lift . throwIRBuilderError

-- | Lift through 'ReaderT'.
instance (MonadIRBuilder m) => MonadIRBuilder (ReaderT r m) where
  getIRBuilderEnv = lift getIRBuilderEnv
  putIRBuilderEnv = lift . putIRBuilderEnv
  throwIRBuilderError = lift . throwIRBuilderError

-- | Lift through 'WriterT'.
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (WriterT w m) where
  getIRBuilderEnv = lift getIRBuilderEnv
  putIRBuilderEnv = lift . putIRBuilderEnv
  throwIRBuilderError = lift . throwIRBuilderError

-- | Lift through 'ExceptT'.
instance (MonadIRBuilder m) => MonadIRBuilder (ExceptT e m) where
  getIRBuilderEnv = lift getIRBuilderEnv
  putIRBuilderEnv = lift . putIRBuilderEnv
  throwIRBuilderError = lift . throwIRBuilderError
