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
ret v = setTerminator (IRet v)

br :: Name -> IRBuilder ()
br n = setTerminator (IBr n)

condbr :: IROperand -> Name -> Name -> IRBuilder ()
condbr v n1 n2 = setTerminator (ICondBr v n1 n2)

switch :: IROperand -> Name -> [(IRConstant, Name)] -> IRBuilder ()
switch v n bs = setTerminator (ISwitch v n bs)

unreachable :: IRBuilder ()
unreachable = setTerminator IUnreachable
