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

-- | Return from the current function with the given operand as the return value.
ret :: IROperand -> IRBuilder ()
ret op = setTerminator (IRet op)

-- | Unconditionally branch to the named block.
br :: Name -> IRBuilder ()
br n = setTerminator (IBr n)

-- | Conditionally branch to one of two blocks based on a boolean operand.
condbr :: IROperand -> Name -> Name -> IRBuilder ()
condbr op n1 n2 = setTerminator (ICondBr op n1 n2)

-- | Branch to one of several blocks based on an integer operand, with a default target.
switch :: IROperand -> Name -> [(IRConstant, Name)] -> IRBuilder ()
switch op n cs = setTerminator (ISwitch op n cs)

-- | Mark the current block as unreachable (undefined behaviour if executed).
unreachable :: IRBuilder ()
unreachable = setTerminator IUnreachable
