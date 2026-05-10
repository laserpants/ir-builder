module LLVM.IRInstruction.Constructors (
  add,
  alloca,
  and,
  ashr,
  bitcast,
  call,
  callVoid,
  fadd,
  fcmp,
  fdiv,
  fmul,
  fneg,
  fsub,
  gep,
  icmp,
  inttoptr,
  load,
  lshr,
  mul,
  or,
  phi,
  ptrtoint,
  sdiv,
  select,
  sext,
  shl,
  srem,
  store,
  sub,
  trunc,
  udiv,
  urem,
  xor,
  zext,
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

-- Arithmetic

add :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
add t a b = emitWithResult t (IAdd t a b)

sub :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
sub t a b = emitWithResult t (ISub t a b)

mul :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
mul t a b = emitWithResult t (IMul t a b)

sdiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
sdiv t a b = emitWithResult t (ISDiv t a b)

udiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
udiv t a b = emitWithResult t (IUDiv t a b)

srem :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
srem t a b = emitWithResult t (ISRem t a b)

urem :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
urem t a b = emitWithResult t (IURem t a b)

-- Bitwise

and :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
and t a b = emitWithResult t (IAnd t a b)

or :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
or t a b = emitWithResult t (IOr t a b)

xor :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
xor t a b = emitWithResult t (IXOr t a b)

shl :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
shl t a b = emitWithResult t (IShl t a b)

lshr :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
lshr t a b = emitWithResult t (ILShr t a b)

ashr :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
ashr t a b = emitWithResult t (IAShr t a b)

-- Floating-point arithmetic

fadd :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fadd t a b = emitWithResult t (IFAdd t a b)

fsub :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fsub t a b = emitWithResult t (IFSub t a b)

fmul :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fmul t a b = emitWithResult t (IFMul t a b)

fdiv :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
fdiv t a b = emitWithResult t (IFDiv t a b)

fneg :: IRType -> IROperand -> IRBuilder IROperand
fneg t a = emitWithResult t (IFNeg t a)

-- Comparison

icmp :: IRICmpCond -> IRType -> IROperand -> IROperand -> IRBuilder IROperand
icmp cc t a b = emitWithResult i1 (IICmp cc t a b)

fcmp :: IRFCmpCond -> IRType -> IROperand -> IROperand -> IRBuilder IROperand
fcmp cc t a b = emitWithResult i1 (IFCmp cc t a b)

-- Memory

alloca :: IRType -> IROperand -> IRBuilder IROperand
alloca t op = emitWithResult (TPtr t) (IAlloca t op)

load :: IRType -> IROperand -> IRBuilder IROperand
load t op = emitWithResult t (ILoad t op)

store :: IROperand -> IROperand -> IRBuilder ()
store op ptr = emitVoid (IStore op ptr)

gep :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
gep t base idx0 idx1 = emitWithResult (TPtr t) (IGep t base idx0 idx1)

-- Casts

bitcast :: IROperand -> IRType -> IRBuilder IROperand
bitcast op t = emitWithResult t (IBitcast op t)

sext :: IROperand -> IRType -> IRBuilder IROperand
sext op t = emitWithResult t (ISext op t)

zext :: IROperand -> IRType -> IRBuilder IROperand
zext op t = emitWithResult t (IZext op t)

trunc :: IROperand -> IRType -> IRBuilder IROperand
trunc op t = emitWithResult t (ITrunc op t)

inttoptr :: IROperand -> IRType -> IRBuilder IROperand
inttoptr op t = emitWithResult t (IInttoptr op t)

ptrtoint :: IROperand -> IRType -> IRBuilder IROperand
ptrtoint op t = emitWithResult t (IPtrtoint op t)

-- Control flow

-- | Call a function that returns a non-void value.
call :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder IROperand
call tm t fn args = emitWithResult t (ICall tm t fn args)

-- | Call a function that returns void.
callVoid :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder ()
callVoid tm t fn args = emitVoid (ICall tm t fn args)

-- Miscellaneous

phi :: IRType -> [(Name, IROperand)] -> IRBuilder IROperand
phi t ops = emitWithResult t (IPhi t ops)

select :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
select t op1 op2 op3 = emitWithResult t (ISelect t op1 op2 op3)
