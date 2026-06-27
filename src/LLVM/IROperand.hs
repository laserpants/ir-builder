{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{- | This module defines the operand and constant value representations used in
LLVM IR, as well as terminator instructions that control flow between basic blocks.

Operands represent values that can be used in instructions, including local
and global references as well as immediate constant values. Terminators are
special instructions that must appear at the end of every basic block to
transfer control flow.
-}
module LLVM.IROperand (
  IRConstant (..),
  IROperand (..),
  IRTerminator (..),
  opComponents,
  constantType,
  operandType,
)
where

import LLVM.IRType (IRName, IRType (..))

{- | Represents constant values in LLVM IR.

Constants are compile-time known values that can be used as operands in
instructions or as initializers for global variables. They are typed and
immutable.

__Constructors:__

* 'CInt': integer constant with the given bit width and value
* 'CFloat': single-precision floating-point constant
* 'CDouble': double-precision floating-point constant
* 'CNull': null pointer constant of the specified type
* 'CStruct': aggregate constant containing a list of field values
* 'CArray': array constant with element type and values
-}
data IRConstant
  = CInt Int Integer
  | CFloat Float
  | CDouble Double
  | CNull IRType
  | CStruct [IRConstant]
  | CArray IRType [IRConstant]
  deriving (Show, Eq, Ord)

{- | Represents operands used in LLVM IR instructions.

An operand is a value that can be used as input to an instruction. It can
reference a local variable (SSA value), a global symbol, or an immediate
constant. All operands are typed.

__Constructors:__

* 'OLocal': reference to a local SSA value (register) with its type
* 'OGlobal': reference to a global symbol with its type
* 'OConstant': immediate constant value
-}
data IROperand
  = OLocal IRType IRName
  | OGlobal IRType IRName
  | OConstant IRConstant
  deriving (Show, Eq, Ord)

{- | Represents terminator instructions in LLVM IR.

Terminators are special instructions that must appear exactly once at the
end of every basic block. They control the flow of execution by either
returning from a function, branching to another block, or indicating
unreachable code.

__Constructors:__

* 'IRet': return from the function; 'Nothing' for @ret void@, 'Just' for a typed return value
* 'IBr': unconditional branch to the named block
* 'ICondBr': conditional branch — if the condition is true, jump to the first block, else the second
* 'ISwitch': multi-way branch on a value, with a default block and a list of cases
* 'IUnreachable': indicates unreachable code (undefined behavior if executed)
-}
data IRTerminator
  = IRet (Maybe IROperand)
  | IBr IRName
  | ICondBr IROperand IRName IRName
  | ISwitch IROperand IRName [(IRConstant, IRName)]
  | IUnreachable
  deriving (Show, Eq, Ord)

{- | Extract the name and type components from an operand.

Returns the name and type for local and global operands. Constant operands
have no name, so they return 'Nothing'.

__Returns:__

* @Just (IRName, IRType)@ for local and global operands
* @Nothing@ for constant operands
-}
opComponents :: IROperand -> Maybe (IRName, IRType)
opComponents =
  \case
    OLocal t name ->
      Just (name, t)
    OGlobal t name ->
      Just (name, t)
    _ ->
      Nothing

-- | Derive the 'IRType' of a constant value.
constantType :: IRConstant -> IRType
constantType =
  \case
    CInt n _ -> TInt n
    CFloat _ -> TFloat
    CDouble _ -> TDouble
    CNull t -> t
    CStruct cs -> TStruct (map constantType cs)
    CArray t cs -> TArray (length cs) t

-- | Derive the 'IRType' of an operand.
operandType :: IROperand -> IRType
operandType =
  \case
    OLocal t _ -> t
    OGlobal t _ -> t
    OConstant c -> constantType c
