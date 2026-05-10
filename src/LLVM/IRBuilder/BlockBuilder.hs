{-# LANGUAGE RecordWildCards #-}

module LLVM.IRBuilder.BlockBuilder (
  BlockBuilder (..),
  appendBlockBuilderItem,
) where

import Common (Name)
import LLVM.IRModule (IRBlockItem)
import LLVM.IROperand (IRTerminator)

data BlockBuilder = BlockBuilder
  { blockBuilderLabel :: Name
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
