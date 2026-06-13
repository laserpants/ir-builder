{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.BlockBuilder (
  BlockBuilder (..),
  appendBlockBuilderItem,
  setBlockBuilderTerminator,
)
where

import LLVM.IRModule (IRBlockItem)
import LLVM.IROperand (IRTerminator)
import LLVM.IRType (IRName)

data BlockBuilder = BlockBuilder
  { blockBuilderLabel :: IRName
  , blockBuilderItems :: [IRBlockItem]
  , blockBuilderTerminator :: Maybe IRTerminator
  }
  deriving (Show, Eq, Ord)

appendBlockBuilderItem :: IRBlockItem -> BlockBuilder -> BlockBuilder
appendBlockBuilderItem item BlockBuilder{..} =
  BlockBuilder
    { blockBuilderItems = blockBuilderItems <> [item]
    , ..
    }

setBlockBuilderTerminator :: IRTerminator -> BlockBuilder -> BlockBuilder
setBlockBuilderTerminator term BlockBuilder{..} =
  BlockBuilder
    { blockBuilderTerminator = Just term
    , ..
    }
