{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilderF (..),
  IRBuilder (..),
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvCurrentFunction,
) where

import Common (Name)
import Control.Monad.State (State)
import Control.Monad.Trans.Free (FreeT)
import Data.Map.Strict (Map)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule (IRBlock, IRDecl, IRFunction, IRGlobal)

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
  { runIRBuilder :: FreeT IRBuilderF (State IRBuilderEnv) a
  }

overBuilderEnvCurrentFunction :: (Maybe IRFunction -> Maybe IRFunction) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fn builderEnvCurrentFunction
    , ..
    }
