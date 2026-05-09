{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilderF (..),
  IRBuilder (..),
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvCurrentFunction,
  execIRBuilder,
  compileModule,
) where

import Common (Name)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Trans.Free (FreeT, iterT)
import Data.Map.Strict (Map)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule
import LLVM.IRRenderer (renderModule, runIRRenderer)

data IRBuilderF next
  = EmitInstr IRInstruction next
  | EmitAnnotation IRAnnotation next
  deriving (Functor)

data IRBuilderEnv = IRBuilderEnv
  { builderEnvFresh :: Int
  , builderEnvCurrentBlock :: Maybe IRBlock
  , builderEnvCurrentFunction :: Maybe IRFunction
  , builderEnvBlocks :: Map Name IRBlock
  , builderEnvFunctions :: Map Name IRFunction
  , builderEnvGlobals :: [IRGlobal]
  , builderEnvDecls :: [IRDecl]
  }
  deriving (Show, Eq, Ord)

emptyIRBuilderEnv :: IRBuilderEnv
emptyIRBuilderEnv =
  IRBuilderEnv
    { builderEnvFresh = 0
    , builderEnvCurrentBlock = Nothing
    , builderEnvCurrentFunction = Nothing
    , builderEnvBlocks = mempty
    , builderEnvFunctions = mempty
    , builderEnvGlobals = mempty
    , builderEnvDecls = mempty
    }

newtype IRBuilder a = IRBuilder
  { unpackIRBuilder :: FreeT IRBuilderF (State IRBuilderEnv) a
  }

overBuilderEnvCurrentFunction :: (Maybe IRFunction -> Maybe IRFunction) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fn builderEnvCurrentFunction
    , ..
    }

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
emitInstruction instr = modify $ overBuilderEnvCurrentFunction (fmap (appendInstr instr))

emitAnnotation :: IRAnnotation -> State IRBuilderEnv ()
emitAnnotation ann = modify $ overBuilderEnvCurrentFunction (fmap (appendAnnotation ann))

execIRBuilder :: IRBuilder a -> State IRBuilderEnv a
execIRBuilder = iterT emit . unpackIRBuilder

finalizeModule :: Name -> IRBuilderEnv -> IRModule
finalizeModule name IRBuilderEnv{..} =
  IRModule
    { moduleName = name
    , moduleDecls = reverse builderEnvDecls
    , moduleGlobals = reverse builderEnvGlobals
    , moduleFunctions = finalizeFunctions IRBuilderEnv{..}
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = undefined

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
 where
  env = execState (execIRBuilder builder) emptyIRBuilderEnv

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runIRRenderer . renderModule . buildModule name
