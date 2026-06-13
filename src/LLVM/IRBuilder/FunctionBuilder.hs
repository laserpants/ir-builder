{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.FunctionBuilder (
  FunctionBuilder (..),
  appendFunctionBuilderBlock,
) where

import LLVM.IRModule (IRAttribute, IRBlock, IRLinkage)
import LLVM.IRType (IRName, IRType)

data FunctionBuilder = FunctionBuilder
  { functionBuilderName :: IRName
  , functionBuilderLinkage :: IRLinkage
  , functionBuilderRetType :: IRType
  , functionBuilderArgs :: [(IRType, IRName)]
  , functionBuilderBlocks :: [IRBlock]
  , functionBuilderAttributes :: [IRAttribute]
  }
  deriving (Show, Eq, Ord)

appendFunctionBuilderBlock :: IRBlock -> FunctionBuilder -> FunctionBuilder
appendFunctionBuilderBlock block FunctionBuilder{..} =
  FunctionBuilder
    { functionBuilderBlocks = functionBuilderBlocks <> [block]
    , ..
    }
