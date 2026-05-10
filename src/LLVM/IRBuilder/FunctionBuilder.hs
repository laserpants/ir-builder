{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.FunctionBuilder (
  FunctionBuilder (..),
  appendInstr,
  appendAnnotation,
  appendFunctionBuilderBlock,
) where

import Common (Name)
import LLVM.IRAnnotation (IRAnnotation)
import LLVM.IRInstruction (IRInstruction)
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

appendInstr :: IRInstruction -> FunctionBuilder -> FunctionBuilder
appendInstr = undefined

appendAnnotation :: IRAnnotation -> FunctionBuilder -> FunctionBuilder
appendAnnotation = undefined

appendFunctionBuilderBlock :: IRBlock -> FunctionBuilder -> FunctionBuilder
appendFunctionBuilderBlock block FunctionBuilder{..} =
  FunctionBuilder
    { functionBuilderBlocks = functionBuilderBlocks <> [block]
    , ..
    }
