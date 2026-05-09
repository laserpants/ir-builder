module LLVM.IRBuilder (IRBuilderF (..), IRBuilder) where

import Control.Monad.Free (Free)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)

data IRBuilderF next
  = EmitInstr IRInstruction next
  | EmitAnnotation IRAnnotation next

--  | FreshName (Name -> next)
--  | LookupVar Name (IROperand -> next)
--  | ...

type IRBuilder = Free IRBuilderF
