module LLVM.IRTerminator.Constructors (
  ret,
  br,
  condbr,
  switch,
  unreachable,
) where

import Common (Name)
import LLVM.IRBuilder (IRBuilder, setTerminator)
import LLVM.IROperand (IRConstant, IROperand, IRTerminator (..))

ret :: IROperand -> IRBuilder ()
ret op = setTerminator (IRet op)

br :: Name -> IRBuilder ()
br n = setTerminator (IBr n)

condbr :: IROperand -> Name -> Name -> IRBuilder ()
condbr op n1 n2 = setTerminator (ICondBr op n1 n2)

switch :: IROperand -> Name -> [(IRConstant, Name)] -> IRBuilder ()
switch op n cs = setTerminator (ISwitch op n cs)

unreachable :: IRBuilder ()
unreachable = setTerminator IUnreachable
