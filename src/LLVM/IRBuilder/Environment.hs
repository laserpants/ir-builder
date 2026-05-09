{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvCurrentFunction,
) where

import Common (Name)
import Data.Map.Strict (Map)
import LLVM.IRModule (IRBlock, IRDecl, IRFunction, IRGlobal)

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

overBuilderEnvCurrentFunction :: (Maybe IRFunction -> Maybe IRFunction) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fn builderEnvCurrentFunction
    , ..
    }
