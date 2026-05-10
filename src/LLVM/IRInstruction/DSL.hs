module LLVM.IRInstruction.DSL (
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
import Control.Monad.Free (liftF)
import LLVM.IRBuilder (IRBuilder, IRBuilderF (..))
import LLVM.IRBuilder.Supply (freshOperand)
import LLVM.IRInstruction (IRFCmpCond, IRICmpCond, IRInstrOp (..), IRInstruction (..), IRTailMarker)
import LLVM.IROperand (IROperand (..), opComponents)
import LLVM.IRType (IRType (..))
import Prelude hiding (and, or)

-- | Emit an instruction that produces a result register.
emitWithResult :: IRType -> IRInstrOp -> IRBuilder IROperand
emitWithResult t op = do
  reg <- freshOperand t
  let instr = IRInstruction{instrResult = opComponents reg, instrOp = op}
  liftF $ EmitInstr instr reg

-- | Emit an instruction that produces no result.
emitVoid :: IRInstrOp -> IRBuilder ()
emitVoid op = do
  let instr = IRInstruction{instrResult = Nothing, instrOp = op}
  liftF $ EmitInstr instr ()

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
icmp cond t a b = emitWithResult (TInt 1) (IICmp cond t a b)

fcmp :: IRFCmpCond -> IRType -> IROperand -> IROperand -> IRBuilder IROperand
fcmp cond t a b = emitWithResult (TInt 1) (IFCmp cond t a b)

-- Memory

alloca :: IRType -> IROperand -> IRBuilder IROperand
alloca t n = emitWithResult (TPtr t) (IAlloca t n)

load :: IRType -> IROperand -> IRBuilder IROperand
load t ptr = emitWithResult t (ILoad t ptr)

store :: IROperand -> IROperand -> IRBuilder ()
store val ptr = emitVoid (IStore val ptr)

gep :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
gep t base idx0 idx1 = emitWithResult (TPtr t) (IGep t base idx0 idx1)

-- Casts

bitcast :: IROperand -> IRType -> IRBuilder IROperand
bitcast v t = emitWithResult t (IBitcast v t)

sext :: IROperand -> IRType -> IRBuilder IROperand
sext v t = emitWithResult t (ISext v t)

zext :: IROperand -> IRType -> IRBuilder IROperand
zext v t = emitWithResult t (IZext v t)

trunc :: IROperand -> IRType -> IRBuilder IROperand
trunc v t = emitWithResult t (ITrunc v t)

inttoptr :: IROperand -> IRType -> IRBuilder IROperand
inttoptr v t = emitWithResult t (IInttoptr v t)

ptrtoint :: IROperand -> IRType -> IRBuilder IROperand
ptrtoint v t = emitWithResult t (IPtrtoint v t)

-- Control flow

-- | Call a function that returns a non-void value.
call :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder IROperand
call tail_ retTy fn args = emitWithResult retTy (ICall tail_ retTy fn args)

-- | Call a function that returns void.
callVoid :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> IRBuilder ()
callVoid tail_ retTy fn args = emitVoid (ICall tail_ retTy fn args)

-- Miscellaneous

phi :: IRType -> [(Name, IROperand)] -> IRBuilder IROperand
phi t incoming = emitWithResult t (IPhi t incoming)

select :: IRType -> IROperand -> IROperand -> IROperand -> IRBuilder IROperand
select t cond t_ f_ = emitWithResult t (ISelect t cond t_ f_)
