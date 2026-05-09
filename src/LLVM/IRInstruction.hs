module LLVM.IRInstruction (
  IRICmpCond (..),
  IRFCmpCond (..),
  IRTailMarker (..),
  IRInstrOp (..),
  IRInstruction (..),
)
where

import Common (Name)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType (..))

-- | icmp instruction condition codes
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

-- | fcmp instruction condition codes
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

-- | IR instruction set
data IRInstrOp
  = IAShr IRType IROperand IROperand
  | IAdd IRType IROperand IROperand
  | IAlloca IRType IROperand
  | IAnd IRType IROperand IROperand
  | IBitcasty IROperand IRType
  | ICall IRTailMarker IRType IROperand [IROperand]
  | IFAdd IRType IROperand IROperand
  | IFCmp IRFCmpCond IRType IROperand IROperand
  | IFDiIROperand IRType IROperand IROperand
  | IFMul IRType IROperand IROperand
  | IFNeg IRType IROperand
  | IFSub IRType IROperand IROperand
  | IGep IRType IROperand IROperand IROperand
  | IICmp IRICmpCond IRType IROperand IROperand
  | IInttoptr IROperand IRType
  | ILShr IRType IROperand IROperand
  | ILoad IRType IROperand
  | IMul IRType IROperand IROperand
  | IOr IRType IROperand IROperand
  | IPhi IRType [(Name, IROperand)]
  | IPtrtointy IROperand IRType
  | ISDiIROperand IRType IROperand IROperand
  | ISRem IRType IROperand IROperand
  | ISelecty IRType IROperand IROperand IROperand
  | ISexty IROperand IRType
  | IShl IRType IROperand IROperand
  | IStore IROperand IROperand
  | ISub IRType IROperand IROperand
  | ITrunc IROperand IRType
  | IUDiIROperand IRType IROperand IROperand
  | IURem IRType IROperand IROperand
  | IXOr IRType IROperand IROperand
  | IZexty IROperand IRType
  deriving (Show, Eq, Ord)

data IRInstruction = IRInstruction
  { instrResult :: Maybe (Name, IRType)
  , instrOp :: IRInstrOp
  }
  deriving (Show, Eq, Ord)
