{-# LANGUAGE StrictData #-}

module LLVM.IRState (IRState (..)) where

import Common (Name)
import Data.Map.Strict (Map)
import LLVM.IRBuilder (IRBlockBuilder (..))
import LLVM.IROperand (IROperand)

data IRState = IRState
  { irStateCurrentBlock :: Name
  , irStateBlocks :: Map Name IRBlockBuilder
  , irStateEnv :: Map Name IROperand
  , irStateCounter :: Int
  }
  deriving (Show, Eq, Ord)
