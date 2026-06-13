module LLVM.IRTerminator.Constructors (
  ret,
  retVoid,
  br,
  condbr,
  switch,
  unreachable,
)
where

import LLVM.IRType (IRName)
import LLVM.IRBuilder (setTerminator)
import LLVM.IRBuilder.Class (MonadIRBuilder)
import LLVM.IROperand (IRConstant, IROperand, IRTerminator (..))

-- | Return from the current function with the given operand as the return value.
ret :: (MonadIRBuilder m) => IROperand -> m ()
ret op = setTerminator (IRet (Just op))

-- | Return from the current function with no value (@ret void@).
retVoid :: (MonadIRBuilder m) => m ()
retVoid = setTerminator (IRet Nothing)

-- | Unconditionally branch to the named block.
br :: (MonadIRBuilder m) => IRName -> m ()
br n = setTerminator (IBr n)

-- | Conditionally branch to one of two blocks based on a boolean operand.
condbr :: (MonadIRBuilder m) => IROperand -> IRName -> IRName -> m ()
condbr op n1 n2 = setTerminator (ICondBr op n1 n2)

-- | Branch to one of several blocks based on an integer operand, with a default target.
switch :: (MonadIRBuilder m) => IROperand -> IRName -> [(IRConstant, IRName)] -> m ()
switch op n cs = setTerminator (ISwitch op n cs)

-- | Mark the current block as unreachable (undefined behaviour if executed).
unreachable :: (MonadIRBuilder m) => m ()
unreachable = setTerminator IUnreachable
