{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLVM.IRRenderer (IRRenderer (..), runIRRenderer, renderModule) where

import Common (Name)
import Control.Monad.State (MonadState, State, evalState)
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (
  IRFCmpCond (..),
  IRICmpCond (..),
  IRInstrOp (..),
  IRInstruction (..),
  IRTailMarker (..),
 )
import LLVM.IRModule (
  IRAttribute (..),
  IRBlock (..),
  IRBlockItem (..),
  IRDecl (..),
  IRFunction (..),
  IRGlobal (..),
  IRLinkage (..),
  IRModule (..),
 )
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRRenderer.State (IRRendererState (..), emptyIRRendererState)
import LLVM.IRType (IRType (..))

newtype IRRenderer a = IRRenderer {unpackIRRenderer :: State IRRendererState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRRendererState
    )

runIRRenderer :: IRRenderer a -> a
runIRRenderer irRenderer = evalState (unpackIRRenderer irRenderer) emptyIRRendererState

renderModule :: IRModule -> IRRenderer Text
renderModule IRModule{moduleDecls, moduleGlobals, moduleFunctions} = do
  decls <- traverse renderDecl moduleDecls
  globs <- traverse renderGlobal moduleGlobals
  funs <- traverse renderFunction moduleFunctions
  pure $ Text.unlines $ concat [decls, globs, funs]

-- | Render a type declaration (e.g., "%Type = type { i32, i32 }")
renderDecl :: IRDecl -> IRRenderer Text
renderDecl IRDecl{declName, declType} = do
  typeStr <- renderType declType
  pure $ "%" <> declName <> " = type " <> typeStr

-- | Render a global variable or external declaration
renderGlobal :: IRGlobal -> IRRenderer Text
renderGlobal =
  \case
    IRString linkage name _bs ->
      let linkageStr = renderLinkage linkage
       in pure $ "@" <> name <> " = " <> linkageStr <> " constant [8 x i8] c\"string\""
    IRConstant linkage name typ val -> do
      typeStr <- renderType typ
      valStr <- renderConstant val
      let linkageStr = renderLinkage linkage
      pure $ "@" <> name <> " = " <> linkageStr <> " constant " <> typeStr <> " " <> valStr
    IRExtern name typ -> do
      typeStr <- renderType typ
      pure $ "declare " <> typeStr <> " @" <> name <> "()"

-- | Render a function definition
renderFunction :: IRFunction -> IRRenderer Text
renderFunction IRFunction{functionName, functionLinkage, functionRetType, functionArgs, functionBlocks, functionAttributes} = do
  retTypeStr <- renderType functionRetType
  argsStr <- renderFunctionArgs functionArgs
  let linkageStr = renderLinkage functionLinkage
  let attrsStr =
        if null functionAttributes
          then ""
          else " " <> Text.unwords (map renderAttribute functionAttributes)
  blocksStr <- mapM renderBlock functionBlocks
  pure $
    "define "
      <> linkageStr
      <> retTypeStr
      <> " @"
      <> functionName
      <> "("
      <> argsStr
      <> ")"
      <> attrsStr
      <> " {\n"
      <> Text.unlines blocksStr
      <> "}\n"

-- | Render a single block
renderBlock :: IRBlock -> IRRenderer Text
renderBlock IRBlock{blockLabel, blockItems, blockTerminator} = do
  itemsStrs <- mapM renderBlockItem blockItems
  termStr <- renderTerminator blockTerminator
  pure $
    blockLabel
      <> ":\n"
      <> Text.unlines itemsStrs
      <> "  "
      <> termStr

-- | Render a block item (instruction or annotation)
renderBlockItem :: IRBlockItem -> IRRenderer Text
renderBlockItem =
  \case
    BlockInstr instr ->
      renderInstruction instr
    BlockAnnotation ann ->
      case ann of
        Comment txt -> pure $ "  ; " <> txt
        CommentBlock txts -> pure $ "  ; " <> Text.unlines (map ("; " <>) txts)

-- | Render an instruction
renderInstruction :: IRInstruction -> IRRenderer Text
renderInstruction IRInstruction{instrResult, instrOp} = do
  opStr <- renderInstrOp instrOp
  case instrResult of
    Nothing ->
      pure $ "  " <> opStr
    Just (name, _typ) ->
      pure $ "  %" <> name <> " = " <> opStr

-- | Render the instruction operation
renderInstrOp :: IRInstrOp -> IRRenderer Text
renderInstrOp =
  \case
    IAdd typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "add " <> tyStr <> " " <> aStr <> ", " <> bStr
    ISub typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "sub " <> tyStr <> " " <> aStr <> ", " <> bStr
    IMul typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "mul " <> tyStr <> " " <> aStr <> ", " <> bStr
    ISDiv typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "sdiv " <> tyStr <> " " <> aStr <> ", " <> bStr
    IUDiv typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "udiv " <> tyStr <> " " <> aStr <> ", " <> bStr
    ISRem typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "srem " <> tyStr <> " " <> aStr <> ", " <> bStr
    IURem typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "urem " <> tyStr <> " " <> aStr <> ", " <> bStr
    IAnd typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "and " <> tyStr <> " " <> aStr <> ", " <> bStr
    IOr typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "or " <> tyStr <> " " <> aStr <> ", " <> bStr
    IXOr typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "xor " <> tyStr <> " " <> aStr <> ", " <> bStr
    IShl typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "shl " <> tyStr <> " " <> aStr <> ", " <> bStr
    ILShr typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "lshr " <> tyStr <> " " <> aStr <> ", " <> bStr
    IAShr typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "ashr " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFAdd typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "fadd " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFSub typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "fsub " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFMul typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "fmul " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFDiv typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "fdiv " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFNeg typ a -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      pure $ "fneg " <> tyStr <> " " <> aStr
    IICmp cond typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "icmp " <> renderICmpCond cond <> " " <> tyStr <> " " <> aStr <> ", " <> bStr
    IFCmp cond typ a b -> do
      tyStr <- renderType typ
      aStr <- renderOperand a
      bStr <- renderOperand b
      pure $ "fcmp " <> renderFCmpCond cond <> " " <> tyStr <> " " <> aStr <> ", " <> bStr
    ILoad _typ ptr -> do
      ptrStr <- renderOperand ptr
      pure $ "load ptr " <> ptrStr
    IStore val ptr -> do
      valStr <- renderOperand val
      ptrStr <- renderOperand ptr
      pure $ "store " <> valStr <> ", ptr " <> ptrStr
    IAlloca typ n -> do
      tyStr <- renderType typ
      nStr <- renderOperand n
      pure $ "alloca " <> tyStr <> ", i32 " <> nStr
    IGep typ base idx0 idx1 -> do
      tyStr <- renderType typ
      baseStr <- renderOperand base
      idx0Str <- renderOperand idx0
      idx1Str <- renderOperand idx1
      pure $ "getelementptr " <> tyStr <> ", ptr " <> baseStr <> ", i32 " <> idx0Str <> ", i32 " <> idx1Str
    IBitcast v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "bitcast " <> vStr <> " to " <> tyStr
    ISext v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "sext " <> vStr <> " to " <> tyStr
    IZext v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "zext " <> vStr <> " to " <> tyStr
    ITrunc v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "trunc " <> vStr <> " to " <> tyStr
    IInttoptr v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "inttoptr " <> vStr <> " to " <> tyStr
    IPtrtoint v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "ptrtoint " <> vStr <> " to " <> tyStr
    ICall marker retTy fn args -> do
      retTyStr <- renderType retTy
      fnStr <- renderOperand fn
      argsStrs <- mapM renderOperand args
      let callStr = "call " <> tailMarkerStr marker <> retTyStr <> " " <> fnStr <> "(" <> Text.intercalate ", " argsStrs <> ")"
      pure callStr
     where
      tailMarkerStr =
        \case
          NoTail -> ""
          Tail -> "tail "
          MustTail -> "musttail "
    IPhi _typ incoming -> do
      incomingStrs <- mapM renderPhiIncoming incoming
      pure $ "phi [" <> Text.intercalate ", " incomingStrs <> "]"
     where
      renderPhiIncoming (blockName, op) = do
        opStr <- renderOperand op
        pure $ opStr <> ", %" <> blockName
    ISelect typ cond t f -> do
      tyStr <- renderType typ
      condStr <- renderOperand cond
      tStr <- renderOperand t
      fStr <- renderOperand f
      pure $ "select i1 " <> condStr <> ", " <> tyStr <> " " <> tStr <> ", " <> tyStr <> " " <> fStr

-- | Render a terminator instruction
renderTerminator :: IRTerminator -> IRRenderer Text
renderTerminator =
  \case
    IRet op -> do
      opStr <- renderOperand op
      pure $ "ret " <> opStr
    IBr target ->
      pure $ "br label %" <> target
    ICondBr cond t f -> do
      condStr <- renderOperand cond
      pure $ "br i1 " <> condStr <> ", label %" <> t <> ", label %" <> f
    ISwitch val default_ cases -> do
      valStr <- renderOperand val
      casesStrs <- mapM renderSwitchCase cases
      pure $ "switch i32 " <> valStr <> ", label %" <> default_ <> " [" <> Text.unlines casesStrs <> "  ]"
     where
      renderSwitchCase (caseVal, caseTarget) = do
        caseValStr <- renderConstant caseVal
        pure $ "    i32 " <> caseValStr <> ", label %" <> caseTarget
    IUnreachable ->
      pure "unreachable"

-- | Render an operand
renderOperand :: IROperand -> IRRenderer Text
renderOperand =
  \case
    OLocal _ name ->
      pure $ "%" <> name
    OGlobal _ name ->
      pure $ "@" <> name
    OConstant c ->
      renderConstant c

-- | Render a type
renderType :: IRType -> IRRenderer Text
renderType =
  \case
    TInt n ->
      pure $ "i" <> Text.pack (show n)
    TFloat ->
      pure "float"
    TDouble ->
      pure "double"
    TVoid ->
      pure "void"
    TPtr _t ->
      pure "ptr"
    TStruct ts -> do
      tsStrs <- mapM renderType ts
      pure $ "{ " <> Text.intercalate ", " tsStrs <> " }"
    TArray n t -> do
      tStr <- renderType t
      pure $ "[" <> Text.pack (show n) <> " x " <> tStr <> "]"
    TFun retTy argTys -> do
      retTyStr <- renderType retTy
      argTyStrs <- mapM renderType argTys
      pure $ retTyStr <> " (" <> Text.intercalate ", " argTyStrs <> ")"
    TNamed name -> pure $ "%" <> name
    TOpaque name -> pure $ "opaque %" <> name
    TVector n t -> do
      tStr <- renderType t
      pure $ "<" <> Text.pack (show n) <> " x " <> tStr <> ">"

-- | Render a constant value
renderConstant :: IRConstant -> IRRenderer Text
renderConstant =
  \case
    CInt _ i ->
      pure $ Text.pack (show i)
    CFloat f ->
      pure $ Text.pack (show f)
    CDouble d ->
      pure $ Text.pack (show d)
    CNull _typ ->
      pure "null"
    CStruct cs -> do
      csStrs <- mapM renderConstant cs
      pure $ "{ " <> Text.intercalate ", " csStrs <> " }"
    CArray _ cs -> do
      csStrs <- mapM renderConstant cs
      pure $ "[ " <> Text.intercalate ", " csStrs <> " ]"

-- | Render function arguments
renderFunctionArgs :: [(IRType, Name)] -> IRRenderer Text
renderFunctionArgs args = do
  argStrs <- mapM (\(typ, name) -> do tyStr <- renderType typ; pure $ tyStr <> " %" <> name) args
  pure $ Text.intercalate ", " argStrs

-- | Render linkage specifier
renderLinkage :: IRLinkage -> Text
renderLinkage =
  \case
    LExternal -> ""
    LInternal -> "internal "
    LPrivate -> "private "

-- | Render integer comparison condition
renderICmpCond :: IRICmpCond -> Text
renderICmpCond =
  \case
    ICmpEq -> "eq"
    ICmpNe -> "ne"
    ICmpUGt -> "ugt"
    ICmpUGe -> "uge"
    ICmpULt -> "ult"
    ICmpULe -> "ule"
    ICmpSGt -> "sgt"
    ICmpSGe -> "sge"
    ICmpSLt -> "slt"
    ICmpSLe -> "sle"

-- | Render floating-point comparison condition
renderFCmpCond :: IRFCmpCond -> Text
renderFCmpCond =
  \case
    FCmpOEq -> "oeq"
    FCmpOGt -> "ogt"
    FCmpOGe -> "oge"
    FCmpOLt -> "olt"
    FCmpOLe -> "ole"
    FCmpONe -> "one"
    FCmpUeq -> "ueq"
    FCmpUGt -> "ugt"
    FCmpUGe -> "uge"
    FCmpULt -> "ult"
    FCmpULe -> "ule"
    FCmpUNe -> "une"
    FCmpOrd -> "ord"
    FCmpUno -> "uno"
    FCmpTrue -> "true"
    FCmpFalse -> "false"

-- | Render an attribute
renderAttribute :: IRAttribute -> Text
renderAttribute =
  \case
    NoReturn -> "noreturn"
    NoUnwind -> "nounwind"
    ReadOnly -> "readonly"
    ReadNone -> "readnone"
    AlwaysInline -> "alwaysinline"
    NoInline -> "noinline"
    TailCall -> "tailcall"
    MustTailCall -> "musttailcall"
    Cold -> "cold"
    Hot -> "hot"
    InlineHint -> "inlinehint"
    NoAlias -> "noalias"
    GC txt -> "gc \"" <> txt <> "\""
