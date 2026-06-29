module LLVM.IRInstruction.Constructors (
  -- * Arithmetic operations
  add,
  sub,
  mul,
  sdiv,
  udiv,
  srem,
  urem,

  -- * Bitwise operations
  and,
  or,
  xor,
  shl,
  lshr,
  ashr,

  -- * Floating-point arithmetic
  fadd,
  fsub,
  fmul,
  fdiv,
  fneg,

  -- * Comparison operations
  icmp,
  fcmp,

  -- * Memory operations
  alloca,
  load,
  store,
  gep,

  -- * Type casts
  bitcast,
  sext,
  zext,
  trunc,
  inttoptr,
  ptrtoint,

  -- * Control flow
  call,
  callVoid,
  callVarArg,
  callVoidVarArg,

  -- * Miscellaneous
  phi,
  select,

  -- * Aggregate operations
  extractValue,
  insertValue,

  -- * Vector operations
  extractElement,
  insertElement,
  shuffleVector,

  -- * Atomics
  atomicRMW,
  cmpXchg,
  fence,

  -- * Freeze
  freeze,
)
where

import LLVM.IRBuilder (emitInstruction)
import LLVM.IRBuilder.Class (MonadIRBuilder)
import LLVM.IRBuilder.Supply (freshOperand)
import LLVM.IRInstruction (IRAtomicOp (..), IRAtomicOrdering (..), IRFCmpCond, IRICmpCond, IRInstrOp (..), IRInstruction (..), IRTailMarker)
import LLVM.IROperand (IROperand (..), opComponents, operandType)
import LLVM.IRType (IRName, IRType (..))
import LLVM.IRType.Constructors (i1)
import Prelude hiding (and, or)

-- | Emit an instruction that produces a result register.
emitWithResult :: (MonadIRBuilder m) => IRType -> IRInstrOp -> m IROperand
emitWithResult t op = do
  reg <- freshOperand t
  emitInstruction $ IRInstruction{instrResult = opComponents reg, instrOp = op, instrMetadata = Nothing}
  return reg

-- | Emit an instruction that produces no result.
emitVoid :: (MonadIRBuilder m) => IRInstrOp -> m ()
emitVoid op = emitInstruction $ IRInstruction{instrResult = Nothing, instrOp = op, instrMetadata = Nothing}

-- * Arithmetic operations

-- | Add two integers.
add :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
add t a b = emitWithResult t (IAdd t a b)

-- | Subtract two integers.
sub :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
sub t a b = emitWithResult t (ISub t a b)

-- | Multiply two integers.
mul :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
mul t a b = emitWithResult t (IMul t a b)

-- | Signed integer division.
sdiv :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
sdiv t a b = emitWithResult t (ISDiv t a b)

-- | Unsigned integer division.
udiv :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
udiv t a b = emitWithResult t (IUDiv t a b)

-- | Signed integer remainder.
srem :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
srem t a b = emitWithResult t (ISRem t a b)

-- | Unsigned integer remainder.
urem :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
urem t a b = emitWithResult t (IURem t a b)

-- * Bitwise operations

-- | Bitwise AND.
and :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
and t a b = emitWithResult t (IAnd t a b)

-- | Bitwise OR.
or :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
or t a b = emitWithResult t (IOr t a b)

-- | Bitwise XOR.
xor :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
xor t a b = emitWithResult t (IXOr t a b)

-- | Bitwise left shift.
shl :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
shl t a b = emitWithResult t (IShl t a b)

-- | Logical right shift.
lshr :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
lshr t a b = emitWithResult t (ILShr t a b)

-- | Arithmetic right shift.
ashr :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
ashr t a b = emitWithResult t (IAShr t a b)

-- * Floating-point arithmetic

-- | Floating-point addition.
fadd :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
fadd t a b = emitWithResult t (IFAdd t a b)

-- | Floating-point subtraction.
fsub :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
fsub t a b = emitWithResult t (IFSub t a b)

-- | Floating-point multiplication.
fmul :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
fmul t a b = emitWithResult t (IFMul t a b)

-- | Floating-point division.
fdiv :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
fdiv t a b = emitWithResult t (IFDiv t a b)

-- | Floating-point negation.
fneg :: (MonadIRBuilder m) => IRType -> IROperand -> m IROperand
fneg t a = emitWithResult t (IFNeg t a)

-- * Comparison operations

-- | Integer comparison with the given condition code.
icmp :: (MonadIRBuilder m) => IRICmpCond -> IRType -> IROperand -> IROperand -> m IROperand
icmp cc t a b = emitWithResult i1 (IICmp cc t a b)

-- | Floating-point comparison with the given condition code.
fcmp :: (MonadIRBuilder m) => IRFCmpCond -> IRType -> IROperand -> IROperand -> m IROperand
fcmp cc t a b = emitWithResult i1 (IFCmp cc t a b)

-- * Memory operations

-- | Allocate space on the stack for a value of the given type.
alloca :: (MonadIRBuilder m) => IRType -> IROperand -> m IROperand
alloca t op = emitWithResult TPtr (IAlloca t op)

-- | Load a value from memory.
load :: (MonadIRBuilder m) => IRType -> IROperand -> m IROperand
load t op = emitWithResult t (ILoad t op)

-- | Store a value to memory.
store :: (MonadIRBuilder m) => IROperand -> IROperand -> m ()
store op ptr = emitVoid (IStore op ptr)

-- | Compute a GEP (get element pointer) address.
gep :: (MonadIRBuilder m) => IRType -> IROperand -> [IROperand] -> m IROperand
gep t base idxs = emitWithResult TPtr (IGep t base idxs)

-- * Type casts

-- | Bitcast value to a different type.
bitcast :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
bitcast op t = emitWithResult t (IBitcast op t)

-- | Sign-extend an integer value.
sext :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
sext op t = emitWithResult t (ISext op t)

-- | Zero-extend an integer value.
zext :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
zext op t = emitWithResult t (IZext op t)

-- | Truncate an integer value.
trunc :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
trunc op t = emitWithResult t (ITrunc op t)

-- | Convert an integer to a pointer.
inttoptr :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
inttoptr op t = emitWithResult t (IInttoptr op t)

-- | Convert a pointer to an integer.
ptrtoint :: (MonadIRBuilder m) => IROperand -> IRType -> m IROperand
ptrtoint op t = emitWithResult t (IPtrtoint op t)

-- * Control flow

-- | Call a function that returns a non-void value.
call :: (MonadIRBuilder m) => IRTailMarker -> IRType -> IROperand -> [IROperand] -> m IROperand
call tm t fn args = emitWithResult t (ICall tm t [] False fn args)

-- | Call a function that returns void.
callVoid :: (MonadIRBuilder m) => IRTailMarker -> IRType -> IROperand -> [IROperand] -> m ()
callVoid tm t fn args = emitVoid (ICall tm t [] False fn args)

{- | Call a variadic function that returns a non-void value.

The @paramTys@ argument lists the fixed parameter types for the function
type annotation emitted in the call instruction, e.g. @[ptr]@ for @printf@.
This annotation is required by the LLVM ABI for variadic calls.
-}
callVarArg :: (MonadIRBuilder m) => IRTailMarker -> IRType -> [IRType] -> IROperand -> [IROperand] -> m IROperand
callVarArg tm t paramTys fn args = emitWithResult t (ICall tm t paramTys True fn args)

{- | Call a variadic function, discarding the return value.

The @paramTys@ argument lists the fixed parameter types for the function
type annotation emitted in the call instruction, e.g. @[ptr]@ for @printf@.
This annotation is required by the LLVM ABI for variadic calls.
-}
callVoidVarArg :: (MonadIRBuilder m) => IRTailMarker -> IRType -> [IRType] -> IROperand -> [IROperand] -> m ()
callVoidVarArg tm t paramTys fn args = emitVoid (ICall tm t paramTys True fn args)

-- * Miscellaneous

-- | Create a phi node for multiple incoming values.
phi :: (MonadIRBuilder m) => IRType -> [(IROperand, IRName)] -> m IROperand
phi t ops = emitWithResult t (IPhi t ops)

-- | Select one of two values based on a condition.
select :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> IROperand -> m IROperand
select t op1 op2 op3 = emitWithResult t (ISelect t op1 op2 op3)

-- * Aggregate operations

-- | Extract a value from an aggregate (struct or array) at the given index path.
extractValue :: (MonadIRBuilder m) => IRType -> IROperand -> [Int] -> m IROperand
extractValue retTy agg idxs = emitWithResult retTy (IExtractValue agg idxs)

-- | Insert a value into an aggregate (struct or array) at the given index path.
insertValue :: (MonadIRBuilder m) => IROperand -> IROperand -> [Int] -> m IROperand
insertValue agg elt idxs = emitWithResult (operandType agg) (IInsertValue agg elt idxs)

-- * Vector operations

-- | Extract a scalar element from a vector.
extractElement :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
extractElement retTy vec idx = emitWithResult retTy (IExtractElement vec idx)

-- | Insert a scalar element into a vector at the given position.
insertElement :: (MonadIRBuilder m) => IROperand -> IROperand -> IROperand -> m IROperand
insertElement vec elt idx = emitWithResult (operandType vec) (IInsertElement vec elt idx)

-- | Shuffle two vectors according to a mask (use -1 for undef slots).
shuffleVector :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> [Int] -> m IROperand
shuffleVector retTy v1 v2 mask = emitWithResult retTy (IShuffleVector v1 v2 mask)

-- * Atomics

-- | Atomic read-modify-write on a memory location.
atomicRMW :: (MonadIRBuilder m) => IRAtomicOrdering -> IRAtomicOp -> IROperand -> IROperand -> m IROperand
atomicRMW ord op ptr val = emitWithResult (operandType val) (IAtomicRMW ord op ptr val)

-- | Atomic compare-and-exchange. Returns @{ <ty>, i1 }@.
cmpXchg :: (MonadIRBuilder m) => IRAtomicOrdering -> IRAtomicOrdering -> IROperand -> IROperand -> IROperand -> m IROperand
cmpXchg succOrd failOrd ptr cmp new_ =
  emitWithResult (TStruct [operandType new_, TInt 1]) (ICmpXchg False succOrd failOrd ptr cmp new_)

-- | Memory fence.
fence :: (MonadIRBuilder m) => IRAtomicOrdering -> m ()
fence ord = emitVoid (IFence ord)

-- * Freeze

-- | Freeze a potentially poison or undef value to an arbitrary fixed value.
freeze :: (MonadIRBuilder m) => IROperand -> m IROperand
freeze op = emitWithResult (operandType op) (IFreeze op)
