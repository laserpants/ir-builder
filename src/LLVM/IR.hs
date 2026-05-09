module LLVM.IR where

import Control.Monad.Free (Free)
import Data.ByteString (ByteString)
import Data.Text (Text)

type Name = Text

--------------------------------------------------------------------------------
-- IRInstruction
--------------------------------------------------------------------------------

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
data IRInstrOpF op ty next
  = IAShr ty op op (op -> next)
  | IAdd ty op op (op -> next)
  | IAlloca ty op (op -> next)
  | IAnd ty op op (op -> next)
  | IBitcasty op ty (op -> next)
  | ICall IRTailMarker ty op [op] (op -> next)
  | IFAdd ty op op (op -> next)
  | IFCmp IRFCmpCond ty op op (op -> next)
  | IFDiop ty op op (op -> next)
  | IFMul ty op op (op -> next)
  | IFNeg ty op (op -> next)
  | IFSub ty op op (op -> next)
  | IGep ty op op op (op -> next)
  | IICmp IRICmpCond ty op op (op -> next)
  | IInttoptr op ty (op -> next)
  | ILShr ty op op (op -> next)
  | ILoad ty op (op -> next)
  | IMul ty op op (op -> next)
  | IOr ty op op (op -> next)
  | IPhi ty [(Name, op)] (op -> next)
  | IPtrtointy op ty (op -> next)
  | ISDiop ty op op (op -> next)
  | ISRem ty op op (op -> next)
  | ISelecty ty op op op (op -> next)
  | ISexty op ty (op -> next)
  | IShl ty op op (op -> next)
  | IStore op op next
  | ISub ty op op (op -> next)
  | ITrunc op ty (op -> next)
  | IUDiop ty op op (op -> next)
  | IURem ty op op (op -> next)
  | IXOr ty op op (op -> next)
  | IZexty op ty (op -> next)

type IRInstrOp = IRInstrOpF IROperand IRType ()

data IRInstruction = IRInstruction
  { instrResult :: Maybe (Name, IRType)
  , instrOp :: IRInstrOp
  }

--------------------------------------------------------------------------------
-- IRType
--------------------------------------------------------------------------------

data IRType
  = TInt Int
  | TFloat
  | TDouble
  | TVoid
  | TFun IRType [IRType]
  | TPtr IRType
  | TStruct [IRType]
  | TArray Int IRType
  | TNamed Name
  | TOpaque Name
  | TVector Int IRType
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- IROperand
--------------------------------------------------------------------------------

data IRConstant
  = CInt Int Integer
  | CFloat Float
  | CDouble Double
  | CNull IRType
  | CStruct [IRConstant]
  | CArray IRType [IRConstant]
  deriving (Show, Eq, Ord)

data IROperand
  = OLocal IRType Name
  | OGlobal IRType Name
  | OConstant IRConstant
  deriving (Show, Eq, Ord)

data IRTerminator
  = IRet IROperand
  | IBr Name
  | ICondBr IROperand Name Name
  | ISwitch IROperand Name [(IRConstant, Name)]
  | IUnreachable
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- IRModule
--------------------------------------------------------------------------------

data IRModule = IRModule
  { modName :: Text
  , modDecls :: [IRDecl]
  , modGlobals :: [IRGlobal]
  , modFunctions :: [IRFunction]
  }

data IRDecl = IRDecl
  { declName :: Name
  , declType :: IRType
  }
  deriving (Show, Eq, Ord)

data IRGlobal
  = IRString Name ByteString
  | IRConstant Name IRType IRConstant
  | IRExtern Name IRType
  deriving (Show, Eq, Ord)

data IRAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline
  | NoInline
  | TailCall
  | MustTailCall
  | Cold
  | Hot
  | InlineHint
  | NoAlias
  | GC Text
  deriving (Show, Eq, Ord)

data IRFunction = IRFunction
  { fnName :: Name
  , fnRetType :: IRType
  , fnArgs :: [(IRType, Name)]
  , fnBlocks :: [IRBlock]
  , fnAttributes :: [IRAttribute]
  }

data IRBlock = IRBlock
  { blockLabel :: Name
  , blockInstructions :: [IRInstruction]
  , blockTerminator :: IRTerminator
  }

--------------------------------------------------------------------------------
-- IRBuilder
--------------------------------------------------------------------------------

type IRBuilder = Free (IRInstrOpF IROperand IRType)
