module LLVM.IRTerminator.Constructors
  ( ret,
    retVoid,
    br,
    condbr,
    switch,
    unreachable,
  )
where

import LLVM.IRBuilder (emitTerminator)
import LLVM.IRBuilder.Class (MonadIRBuilder)
import LLVM.IROperand (IRConstant, IROperand, IRTerminator (..))
import LLVM.IRType (IRName)

-- | Return from the current function with the given operand as the return value.
ret :: (MonadIRBuilder m) => IROperand -> m ()
ret op = emitTerminator (IRet (Just op))

-- | Return from the current function with no value (@ret void@).
retVoid :: (MonadIRBuilder m) => m ()
retVoid = emitTerminator (IRet Nothing)

-- | Unconditionally branch to the named block.
br :: (MonadIRBuilder m) => IRName -> m ()
br n = emitTerminator (IBr n)

-- | Conditionally branch to one of two blocks based on a boolean operand.
condbr :: (MonadIRBuilder m) => IROperand -> IRName -> IRName -> m ()
condbr op n1 n2 = emitTerminator (ICondBr op n1 n2)

-- | Branch to one of several blocks based on an integer operand, with a default target.
switch :: (MonadIRBuilder m) => IROperand -> IRName -> [(IRConstant, IRName)] -> m ()
switch op n cs = emitTerminator (ISwitch op n cs)

-- | Mark the current block as unreachable (undefined behaviour if executed).
unreachable :: (MonadIRBuilder m) => m ()
unreachable = emitTerminator IUnreachable
