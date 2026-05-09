{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.State (IRBuilderState (..)) where

import Common (Name)
import Data.Map.Strict (Map)
import LLVM.IRBuilder (IRBlockBuilder (..))
import LLVM.IROperand (IROperand)

data IRBuilderState = IRBuilderState
  { irBuilderStateCurrentBlock :: Name
  , irBuilderStateBlocks :: Map Name IRBlockBuilder
  , irBuilderStateEnv :: Map Name IROperand
  , irBuilderStateCounter :: Int
  }
  deriving (Show, Eq, Ord)
