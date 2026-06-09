{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvFreshReg,
  overBuilderEnvFreshLabel,
  overBuilderEnvCurrentBlock,
  mapBuilderEnvCurrentBlock,
  setBuilderEnvCurrentBlock,
  overBuilderEnvCurrentFunction,
  mapBuilderEnvCurrentFunction,
  clearBuilderEnvCurrentBlock,
  overBuilderEnvGlobals,
  appendBuilderEnvGlobals,
)
where

import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..))
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..))
import LLVM.IRModule (IRBlock, IRDecl, IRFunction, IRGlobal)

data IRBuilderEnv = IRBuilderEnv
  { builderEnvFreshReg :: Int
  , builderEnvFreshLabel :: Int
  , builderEnvCurrentBlock :: Maybe BlockBuilder
  , builderEnvCurrentFunction :: Maybe FunctionBuilder
  , builderEnvBlocks :: [IRBlock]
  , builderEnvFunctions :: [IRFunction]
  , builderEnvGlobals :: [IRGlobal]
  , builderEnvDecls :: [IRDecl]
  }
  deriving (Show, Eq, Ord)

emptyIRBuilderEnv :: IRBuilderEnv
emptyIRBuilderEnv =
  IRBuilderEnv
    { builderEnvFreshReg = 0
    , builderEnvFreshLabel = 0
    , builderEnvCurrentBlock = Nothing
    , builderEnvCurrentFunction = Nothing
    , builderEnvBlocks = mempty
    , builderEnvFunctions = mempty
    , builderEnvGlobals = mempty
    , builderEnvDecls = mempty
    }

overBuilderEnvFreshReg :: (Int -> Int) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvFreshReg fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvFreshReg = fn builderEnvFreshReg
    , ..
    }

overBuilderEnvFreshLabel :: (Int -> Int) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvFreshLabel fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvFreshLabel = fn builderEnvFreshLabel
    , ..
    }

overBuilderEnvCurrentBlock :: (Maybe BlockBuilder -> Maybe BlockBuilder) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentBlock fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentBlock = fn builderEnvCurrentBlock
    , ..
    }

mapBuilderEnvCurrentBlock :: (BlockBuilder -> BlockBuilder) -> IRBuilderEnv -> IRBuilderEnv
mapBuilderEnvCurrentBlock fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentBlock = fmap fn builderEnvCurrentBlock
    , ..
    }

setBuilderEnvCurrentBlock :: BlockBuilder -> IRBuilderEnv -> IRBuilderEnv
setBuilderEnvCurrentBlock block = overBuilderEnvCurrentBlock (const (Just block))

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

clearBuilderEnvCurrentBlock :: IRBuilderEnv -> IRBuilderEnv
clearBuilderEnvCurrentBlock IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentBlock = Nothing
    , ..
    }

overBuilderEnvGlobals :: ([IRGlobal] -> [IRGlobal]) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvGlobals fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvGlobals = fn builderEnvGlobals
    , ..
    }

appendBuilderEnvGlobals :: [IRGlobal] -> IRBuilderEnv -> IRBuilderEnv
appendBuilderEnvGlobals globals = overBuilderEnvGlobals (<> globals)
