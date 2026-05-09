{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder (
  IRBuilderF (..),
  IRBuilder,
  IRBlockBuilder (..),
) where

import Control.Monad.Free (Free)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IROperand (IRTerminator)

data IRBuilderF next
  = EmitInstr IRInstruction next
  | EmitAnnotation IRAnnotation next

--  | FreshName (Name -> next)
--  | LookupVar Name (IROperand -> next)
--  | ...

type IRBuilder = Free IRBuilderF

data IRBlockBuilder = IRBlockBuilder
  { irBlockBuilderInstructions :: [IRInstruction]
  , irBlockBuilderTerminator :: Maybe IRTerminator
  , irBlockBuilderAnnotations :: [IRAnnotation]
  }
  deriving (Show, Eq, Ord)
