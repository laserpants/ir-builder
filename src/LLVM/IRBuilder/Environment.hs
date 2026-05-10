{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvFresh,
  overBuilderEnvCurrentFunction,
  mapBuilderEnvCurrentFunction,
) where

import Common (Name)
import Data.Map.Strict (Map)
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..))
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..))
import LLVM.IRModule (IRBlock, IRDecl, IRFunction, IRGlobal)

data IRBuilderEnv = IRBuilderEnv
  { builderEnvFresh :: Int
  , builderEnvCurrentBlock :: Maybe BlockBuilder
  , builderEnvCurrentFunction :: Maybe FunctionBuilder
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

overBuilderEnvFresh :: (Int -> Int) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvFresh fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvFresh = fn builderEnvFresh
    , ..
    }

overBuilderEnvCurrentFunction :: (Maybe FunctionBuilder -> Maybe FunctionBuilder) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fn builderEnvCurrentFunction
    , ..
    }

mapBuilderEnvCurrentFunction :: (FunctionBuilder -> FunctionBuilder) -> IRBuilderEnv -> IRBuilderEnv
mapBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fmap fn builderEnvCurrentFunction
    , ..
    }
