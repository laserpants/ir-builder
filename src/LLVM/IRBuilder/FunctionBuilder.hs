{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.FunctionBuilder (
  FunctionBuilder (..),
  appendFunctionBuilderBlock,
) where

import Common (Name)
import LLVM.IRModule (IRAttribute, IRBlock, IRLinkage)
import LLVM.IRType (IRType)

data FunctionBuilder = FunctionBuilder
  { functionBuilderName :: Name
  , functionBuilderLinkage :: IRLinkage
  , functionBuilderRetType :: IRType
  , functionBuilderArgs :: [(IRType, Name)]
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
