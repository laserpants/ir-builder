{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilderF (..),
  IRBuilder (..),
  IRBuilderEnv (..),
  compileModule,
) where

import Common (Name)
import Control.Monad.State (MonadState, State, execState, modify)
import Control.Monad.Trans.Free (FreeT, MonadFree, iterT)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv, mapBuilderEnvCurrentFunction)
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule (IRFunction, IRModule (..), appendAnnotation, appendInstr)
import LLVM.IRRenderer (renderModule, runIRRenderer)

data IRBuilderF next
  = EmitInstr IRInstruction next
  | EmitAnnotation IRAnnotation next
  deriving (Functor)

newtype IRBuilder a = IRBuilder
  { unpackIRBuilder :: FreeT IRBuilderF (State IRBuilderEnv) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRBuilderEnv
    , MonadFree IRBuilderF
    )

emit :: IRBuilderF (State IRBuilderEnv a) -> State IRBuilderEnv a
emit =
  \case
    EmitInstr instr next -> do
      emitInstruction instr
      next
    EmitAnnotation ann next -> do
      emitAnnotation ann
      next

emitInstruction :: IRInstruction -> State IRBuilderEnv ()
emitInstruction instr = modify $ mapBuilderEnvCurrentFunction (appendInstr instr)

emitAnnotation :: IRAnnotation -> State IRBuilderEnv ()
emitAnnotation ann = modify $ mapBuilderEnvCurrentFunction (appendAnnotation ann)

execIRBuilder :: IRBuilder a -> State IRBuilderEnv a
execIRBuilder = iterT emit . unpackIRBuilder

finalizeModule :: Name -> IRBuilderEnv -> IRModule
finalizeModule name env@IRBuilderEnv{..} =
  IRModule
    { moduleName = name
    , moduleDecls = reverse builderEnvDecls
    , moduleGlobals = reverse builderEnvGlobals
    , moduleFunctions = finalizeFunctions env
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = undefined

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
 where
  env = execState (execIRBuilder builder) emptyIRBuilderEnv

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runIRRenderer . renderModule . buildModule name
