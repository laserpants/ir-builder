{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{- |
This module defines the operand and constant value representations used in
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
)
where

import Common (Name)
import LLVM.IRType (IRType (..))

{- |
Represents constant values in LLVM IR.

Constants are compile-time known values that can be used as operands in
instructions or as initializers for global variables. They are typed and
immutable.

==== __Constructors__

[@CInt Int Integer@] Integer constant with bit width and value
[@CFloat Float@] Single-precision floating-point constant
[@CDouble Double@] Double-precision floating-point constant
[@CNull IRType@] Null pointer constant of the specified type
[@CStruct [IRConstant]@] Aggregate constant containing a list of field values
[@CArray IRType [IRConstant]@] Array constant with element type and values
-}
data IRConstant
  = CInt Int Integer
  | CFloat Float
  | CDouble Double
  | CNull IRType
  | CStruct [IRConstant]
  | CArray IRType [IRConstant]
  deriving (Show, Eq, Ord)

{- |
Represents operands used in LLVM IR instructions.

An operand is a value that can be used as input to an instruction. It can
reference a local variable (SSA value), a global symbol, or an immediate
constant. All operands are typed.

==== __Constructors__

[@OLocal IRType Name@] Reference to a local SSA value (register) with its type
[@OGlobal IRType Name@] Reference to a global symbol with its type
[@OConstant IRConstant@] Immediate constant value
-}
data IROperand
  = OLocal IRType Name
  | OGlobal IRType Name
  | OConstant IRConstant
  deriving (Show, Eq, Ord)

{- |
Represents terminator instructions in LLVM IR.

Terminators are special instructions that must appear exactly once at the
end of every basic block. They control the flow of execution by either
returning from a function, branching to another block, or indicating
unreachable code.

==== __Constructors__

[@IRet IROperand@] Return from function with the given value
[@IBr Name@] Unconditional branch to the named block
[@ICondBr IROperand Name Name@] Conditional branch: if condition, then true block, else false block
[@ISwitch IROperand Name [(IRConstant, Name)]@] Multi-way branch on value with default block and case list
[@IUnreachable@] Indicates unreachable code (undefined behavior if reached)
-}
data IRTerminator
  = IRet IROperand
  | IBr Name
  | ICondBr IROperand Name Name
  | ISwitch IROperand Name [(IRConstant, Name)]
  | IUnreachable
  deriving (Show, Eq, Ord)

{- |
Extract the name and type components from an operand.

This function returns the name and type for local and global operands,
returning 'Nothing' for constant operands which don't have names.

==== __Parameters__

* 'IROperand' - The operand to extract components from

==== __Returns__

* @Just (Name, IRType)@ for local and global operands
* @Nothing@ for constant operands
-}
opComponents :: IROperand -> Maybe (Name, IRType)
opComponents =
  \case
    OLocal t name ->
      Just (name, t)
    OGlobal t name ->
      Just (name, t)
    _ ->
      Nothing
