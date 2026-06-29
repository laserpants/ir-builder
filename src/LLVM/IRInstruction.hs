{-# LANGUAGE StrictData #-}

module LLVM.IRInstruction (
  IRICmpCond (..),
  IRFCmpCond (..),
  IRTailMarker (..),
  IRAtomicOrdering (..),
  IRAtomicOp (..),
  IRInstrOp (..),
  IRInstruction (..),
)
where

import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRName, IRType (..))

-- | Condition codes for the @icmp@ instruction.
data IRICmpCond
  = ICmpEq
  | ICmpNe
  | ICmpUGt
  | ICmpUGe
  | ICmpULt
  | ICmpULe
  | ICmpSGt
  | ICmpSGe
  | ICmpSLt
  | ICmpSLe
  deriving (Show, Eq, Ord)

-- | Condition codes for the @fcmp@ instruction.
data IRFCmpCond
  = FCmpOEq
  | FCmpOGt
  | FCmpOGe
  | FCmpOLt
  | FCmpOLe
  | FCmpONe
  | FCmpUeq
  | FCmpUGt
  | FCmpUGe
  | FCmpULt
  | FCmpULe
  | FCmpUNe
  | FCmpOrd
  | FCmpUno
  | FCmpTrue
  | FCmpFalse
  deriving (Show, Eq, Ord)

data IRTailMarker
  = NoTail
  | Tail
  | MustTail
  deriving (Show, Eq, Ord)

-- | Atomic memory ordering constraints.
data IRAtomicOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcqRel
  | SeqCst
  deriving (Show, Eq, Ord)

-- | Operations for the 'IAtomicRMW' instruction.
data IRAtomicOp
  = ARMWXchg
  | ARMWAdd
  | ARMWSub
  | ARMWAnd
  | ARMWNand
  | ARMWOr
  | ARMWXor
  | ARMWMax
  | ARMWMin
  | ARMWUMax
  | ARMWUMin
  | ARMWFAdd
  | ARMWFSub
  | ARMWFMax
  | ARMWFMin
  deriving (Show, Eq, Ord)

-- | All supported IR instruction operations.
data IRInstrOp
  = IAShr IRType IROperand IROperand
  | IAdd IRType IROperand IROperand
  | IAlloca IRType IROperand
  | IAnd IRType IROperand IROperand
  | IBitcast IROperand IRType
  | ICall IRTailMarker IRType [IRType] Bool IROperand [IROperand]
  | IFAdd IRType IROperand IROperand
  | IFCmp IRFCmpCond IRType IROperand IROperand
  | IFDiv IRType IROperand IROperand
  | IFMul IRType IROperand IROperand
  | IFNeg IRType IROperand
  | IFSub IRType IROperand IROperand
  | IGep IRType IROperand [IROperand]
  | IICmp IRICmpCond IRType IROperand IROperand
  | IInttoptr IROperand IRType
  | ILShr IRType IROperand IROperand
  | ILoad IRType IROperand
  | IMul IRType IROperand IROperand
  | IOr IRType IROperand IROperand
  | IPhi IRType [(IROperand, IRName)]
  | IPtrtoint IROperand IRType
  | ISDiv IRType IROperand IROperand
  | ISRem IRType IROperand IROperand
  | ISelect IRType IROperand IROperand IROperand
  | ISext IROperand IRType
  | IShl IRType IROperand IROperand
  | IStore IROperand IROperand
  | ISub IRType IROperand IROperand
  | ITrunc IROperand IRType
  | IUDiv IRType IROperand IROperand
  | IURem IRType IROperand IROperand
  | IXOr IRType IROperand IROperand
  | IZext IROperand IRType
  | IExtractValue IROperand [Int]
  | IInsertValue IROperand IROperand [Int]
  | IExtractElement IROperand IROperand
  | IInsertElement IROperand IROperand IROperand
  | IShuffleVector IROperand IROperand [Int]
  | IAtomicRMW IRAtomicOrdering IRAtomicOp IROperand IROperand
  | ICmpXchg Bool IRAtomicOrdering IRAtomicOrdering IROperand IROperand IROperand
  | IFence IRAtomicOrdering
  | IFreeze IROperand
  deriving (Show, Eq, Ord)

data IRInstruction a = IRInstruction
  { instrResult :: Maybe (IRName, IRType)
  , instrOp :: IRInstrOp
  , instrMetadata :: a
  }
  deriving (Show, Eq, Ord)
