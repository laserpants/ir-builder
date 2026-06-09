module LLVM.IRTerminator.Constructors (
  ret,
  br,
  condbr,
  switch,
  unreachable,
)
where

import Common (Name)
import LLVM.IRBuilder (setTerminator)
import LLVM.IRBuilder.Class (MonadIRBuilder)
import LLVM.IROperand (IRConstant, IROperand, IRTerminator (..))

-- | Return from the current function with the given operand as the return value.
ret :: (MonadIRBuilder m) => IROperand -> m ()
ret op = setTerminator (IRet op)

-- | Unconditionally branch to the named block.
br :: (MonadIRBuilder m) => Name -> m ()
br n = setTerminator (IBr n)

-- | Conditionally branch to one of two blocks based on a boolean operand.
condbr :: (MonadIRBuilder m) => IROperand -> Name -> Name -> m ()
condbr op n1 n2 = setTerminator (ICondBr op n1 n2)

-- | Branch to one of several blocks based on an integer operand, with a default target.
switch :: (MonadIRBuilder m) => IROperand -> Name -> [(IRConstant, Name)] -> m ()
switch op n cs = setTerminator (ISwitch op n cs)

-- | Mark the current block as unreachable (undefined behaviour if executed).
unreachable :: (MonadIRBuilder m) => m ()
unreachable = setTerminator IUnreachable
