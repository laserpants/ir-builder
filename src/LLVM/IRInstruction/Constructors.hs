module LLVM.IRInstruction.Constructors (
  -- * Arithmetic Operations
  add,
  sub,
  mul,
  sdiv,
  udiv,
  srem,
  urem,

  -- * Bitwise Operations
  and,
  or,
  xor,
  shl,
  lshr,
  ashr,

  -- * Floating-Point Arithmetic
  fadd,
  fsub,
  fmul,
  fdiv,
  fneg,

  -- * Comparison Operations
  icmp,
  fcmp,

  -- * Memory Operations
  alloca,
  load,
  store,
  gep,

  -- * Type Casts
  bitcast,
  sext,
  zext,
  trunc,
  inttoptr,
  ptrtoint,

  -- * Control Flow
  call,
  callVoid,

  -- * Miscellaneous
  phi,
  select,
)
where

import Common (Name)
import LLVM.IRBuilder (IRBuilder, emitInstruction)
import LLVM.IRBuilder.Supply (freshOperand)
import LLVM.IRInstruction (IRFCmpCond, IRICmpCond, IRInstrOp (..), IRInstruction (..), IRTailMarker)
import LLVM.IROperand (IROperand (..), opComponents)
import LLVM.IRType (IRType (..))
import LLVM.IRType.Constructors (i1)
import Prelude hiding (and, or)

-- | Emit an instruction that produces a result register.
emitWithResult :: IRType -> IRInstrOp -> IRBuilder IROperand
emitWithResult t op = do
  reg <- freshOperand t
  emitInstruction $ IRInstruction{instrResult = opComponents reg, instrOp = op}
  return reg

-- | Emit an instruction that produces no result.
emitVoid :: IRInstrOp -> IRBuilder ()
emitVoid op = emitInstruction $ IRInstruction{instrResult = Nothing, instrOp = op}

-- * Arithmetic Operations

-- | Add two integers.
add :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
add t a b = emitWithResult t (IAdd t a b)

-- | Subtract two integers.
sub :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
sub t a b = emitWithResult t (ISub t a b)

-- | Multiply two integers.
mul :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
mul t a b = emitWithResult t (IMul t a b)

-- | Signed integer division.
sdiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
sdiv t a b = emitWithResult t (ISDiv t a b)

-- | Unsigned integer division.
udiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
udiv t a b = emitWithResult t (IUDiv t a b)

-- | Signed integer remainder.
srem :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
srem t a b = emitWithResult t (ISRem t a b)

-- | Unsigned integer remainder.
urem :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
urem t a b = emitWithResult t (IURem t a b)

-- * Bitwise Operations

-- | Bitwise AND.
and :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
and t a b = emitWithResult t (IAnd t a b)

-- | Bitwise OR.
or :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
or t a b = emitWithResult t (IOr t a b)

-- | Bitwise XOR.
xor :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
xor t a b = emitWithResult t (IXOr t a b)

-- | Bitwise left shift.
shl :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
shl t a b = emitWithResult t (IShl t a b)

-- | Logical right shift.
lshr :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
lshr t a b = emitWithResult t (ILShr t a b)

-- | Arithmetic right shift.
ashr :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
ashr t a b = emitWithResult t (IAShr t a b)

-- * Floating-Point Arithmetic

-- | Floating-point addition.
fadd :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fadd t a b = emitWithResult t (IFAdd t a b)

-- | Floating-point subtraction.
fsub :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fsub t a b = emitWithResult t (IFSub t a b)

-- | Floating-point multiplication.
fmul :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fmul t a b = emitWithResult t (IFMul t a b)

-- | Floating-point division.
fdiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fdiv t a b = emitWithResult t (IFDiv t a b)

-- | Floating-point negation.
fneg :: IRType -> IROperand -> IRBuilder IROperand
fneg t a = emitWithResult t (IFNeg t a)

-- * Comparison Operations

-- | Integer comparison with the given condition code.
icmp :: IRICmpCond -> IRType -> IROperand -> IROperand -> IRBuilder IROperand
icmp cc t a b = emitWithResult i1 (IICmp cc t a b)

-- | Floating-point comparison with the given condition code.
fcmp :: IRFCmpCond -> IRType -> IROperand -> IROperand -> IRBuilder IROperand
fcmp cc t a b = emitWithResult i1 (IFCmp cc t a b)

-- * Memory Operations

-- | Allocate space on the stack for a value of the given type.
alloca :: IRType -> IROperand -> IRBuilder IROperand
alloca t op = emitWithResult TPtr (IAlloca t op)

-- | Load a value from memory.
load :: IRType -> IROperand -> IRBuilder IROperand
load t op = emitWithResult t (ILoad t op)

-- | Store a value to memory.
store :: IROperand -> IROperand -> IRBuilder ()
store op ptr = emitVoid (IStore op ptr)

-- | Compute a GEP (get element pointer) address.
gep :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
gep t base idx0 idx1 = emitWithResult TPtr (IGep t base idx0 idx1)

-- * Type Casts

-- | Bitcast value to a different type.
bitcast :: IROperand -> IRType -> IRBuilder IROperand
bitcast op t = emitWithResult t (IBitcast op t)

-- | Sign-extend an integer value.
sext :: IROperand -> IRType -> IRBuilder IROperand
sext op t = emitWithResult t (ISext op t)

-- | Zero-extend an integer value.
zext :: IROperand -> IRType -> IRBuilder IROperand
zext op t = emitWithResult t (IZext op t)

-- | Truncate an integer value.
trunc :: IROperand -> IRType -> IRBuilder IROperand
trunc op t = emitWithResult t (ITrunc op t)

-- | Convert an integer to a pointer.
inttoptr :: IROperand -> IRType -> IRBuilder IROperand
inttoptr op t = emitWithResult t (IInttoptr op t)

-- | Convert a pointer to an integer.
ptrtoint :: IROperand -> IRType -> IRBuilder IROperand
ptrtoint op t = emitWithResult t (IPtrtoint op t)

-- * Control Flow

-- | Call a function that returns a non-void value.
call :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder IROperand
call tm t fn args = emitWithResult t (ICall tm t fn args)

-- | Call a function that returns void.
callVoid :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder ()
callVoid tm t fn args = emitVoid (ICall tm t fn args)

-- * Miscellaneous

-- | Create a phi node for multiple incoming values.
phi :: IRType -> [(Name, IROperand)] -> IRBuilder IROperand
phi t ops = emitWithResult t (IPhi t ops)

-- | Select one of two values based on a condition.
select :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
select t op1 op2 op3 = emitWithResult t (ISelect t op1 op2 op3)
