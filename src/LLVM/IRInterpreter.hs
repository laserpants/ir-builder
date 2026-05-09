{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module LLVM.IRInterpreter (interpret) where

import Control.Monad.Free (foldFree)
import Control.Monad.State (MonadState, State)
import LLVM.IRBuilder (IRBuilder, IRBuilderF (..))
import LLVM.IRState (IRState)

newtype IRInterpreter a = IRInterpreter
  { runIRInterpreter :: State IRState a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRState
    )

step :: IRBuilderF a -> IRInterpreter a
step =
  \case
    EmitInstr instr next ->
      undefined
    EmitAnnotation ann next ->
      undefined

interpret :: IRBuilder a -> IRInterpreter a
interpret = foldFree step
