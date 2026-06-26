{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
This module provides functionality for rendering LLVM IR data structures into their
textual LLVM IR representation. It handles the conversion of IR modules, functions,
blocks, instructions, types, and operands into properly formatted LLVM assembly code.

The primary entry point is 'renderModule', which takes an 'IRModule' and produces
the complete textual IR output as a pure 'Text' value.
-}
module LLVM.IRRenderer (renderModule) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64)
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
  IRFunction (..),
  IRGlobal (..),
  IRLinkage (..),
  IRModule (..),
  IRTypeDecl (..),
 )
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..), constantType, operandType)
import LLVM.IRType (IRName, IRType (..))
import Text.Printf (printf)

{- | Returns True if the name needs quoting in LLVM IR.
Safe (unquoted) identifiers match [-a-zA-Z$._][-a-zA-Z$._0-9]*.
-}
needsQuoting :: IRName -> Bool
needsQuoting n = case Text.uncons n of
  Nothing -> True
  Just (h, t) -> not (isSafeHead h) || Text.any (not . isSafeBody) t
 where
  isSafeHead c = isAlphaNum c || c == '-' || c == '$' || c == '.' || c == '_'
  isSafeBody c = isSafeHead c

-- | Wrap a name in double-quotes if required by LLVM IR; leave it bare otherwise.
quoteIfNeeded :: IRName -> Text
quoteIfNeeded n
  | needsQuoting n = "\"" <> n <> "\""
  | otherwise = n

{- |
Render an LLVM IR module to its textual representation.

Processes all module components in order:

1. Type declarations
2. Global variables and constants
3. Function definitions

The output is well-formed LLVM IR that can be written to a @.ll@ file or passed
to LLVM tools such as @llc@ or @opt@.
-}
renderModule :: IRModule -> Text
renderModule IRModule{moduleTypeDecls, moduleGlobals, moduleFunctions} =
  let items =
        map renderTypeDecl moduleTypeDecls
          <> map renderGlobal moduleGlobals
          <> map renderFunction moduleFunctions
   in if null items then "" else Text.intercalate "\n" items <> "\n"

-- | Render a type declaration (e.g. @%Node = type { i32, ptr }@).
renderTypeDecl :: IRTypeDecl -> Text
renderTypeDecl IRTypeDecl{typeDeclName, typeDeclType} =
  "%" <> quoteIfNeeded typeDeclName <> " = type " <> renderType typeDeclType <> "\n"

-- | Render a global variable, string constant, or external declaration.
renderGlobal :: IRGlobal -> Text
renderGlobal = \case
  IRString linkage name bs ->
    "@"
      <> quoteIfNeeded name
      <> " = "
      <> renderLinkage linkage
      <> "constant ["
      <> Text.pack (show (BS.length bs))
      <> " x i8] c\""
      <> renderByteStringLiteral bs
      <> "\"\n"
  IRConstant linkage name typ val ->
    "@"
      <> quoteIfNeeded name
      <> " = "
      <> renderLinkage linkage
      <> " constant "
      <> renderType typ
      <> " "
      <> renderConstant val
      <> "\n"
  IRVar linkage name typ val ->
    "@"
      <> quoteIfNeeded name
      <> " = "
      <> renderLinkage linkage
      <> "global "
      <> renderType typ
      <> " "
      <> renderConstant val
      <> "\n"
  IRExtern name retTy argTys isVariadic ->
    let argTyStrs = map renderType argTys
        suffix =
          if isVariadic
            then if null argTyStrs then "..." else ", ..."
            else ""
     in "declare "
          <> renderType retTy
          <> " @"
          <> quoteIfNeeded name
          <> "("
          <> Text.intercalate ", " argTyStrs
          <> suffix
          <> ")\n"

-- | Render a function definition.
renderFunction :: IRFunction -> Text
renderFunction IRFunction{functionName, functionLinkage, functionRetType, functionArgs, functionBlocks, functionAttributes} =
  let attrsStr =
        if null functionAttributes
          then ""
          else " " <> Text.unwords (map renderAttribute functionAttributes)
   in "define "
        <> renderLinkage functionLinkage
        <> renderType functionRetType
        <> " @"
        <> quoteIfNeeded functionName
        <> "("
        <> renderFunctionArgs functionArgs
        <> ")"
        <> attrsStr
        <> " {\n"
        <> Text.unlines (map renderBlock functionBlocks)
        <> "}\n"

-- | Render a single basic block.
renderBlock :: IRBlock -> Text
renderBlock IRBlock{blockLabel, blockItems, blockTerminator} =
  quoteIfNeeded blockLabel
    <> ":\n"
    <> Text.unlines (map renderBlockItem blockItems)
    <> "  "
    <> renderTerminator blockTerminator

-- | Render a block item (instruction or comment annotation).
renderBlockItem :: IRBlockItem -> Text
renderBlockItem =
  \case
    BlockInstr instr ->
      renderInstruction instr
    BlockAnnotation ann ->
      case ann of
        Comment txt -> "  ; " <> txt
        CommentBlock txts -> Text.unlines [("  ; " <>) line | line <- txts]

-- | Render an instruction with its optional result register and inline comment.
renderInstruction :: IRInstruction (Maybe Text) -> Text
renderInstruction IRInstruction{instrResult, instrOp, instrMetadata} =
  let opStr = renderInstrOp instrOp
      baseStr = case instrResult of
        Nothing -> "  " <> opStr
        Just (name, _) -> "  %" <> quoteIfNeeded name <> " = " <> opStr
   in case instrMetadata of
        Nothing -> baseStr
        Just comment -> baseStr <> "  ; " <> comment

-- | Render an instruction operation to its mnemonic and operands.
renderInstrOp :: IRInstrOp -> Text
renderInstrOp = \case
  IAdd typ a b -> binOp "add" typ a b
  ISub typ a b -> binOp "sub" typ a b
  IMul typ a b -> binOp "mul" typ a b
  ISDiv typ a b -> binOp "sdiv" typ a b
  IUDiv typ a b -> binOp "udiv" typ a b
  ISRem typ a b -> binOp "srem" typ a b
  IURem typ a b -> binOp "urem" typ a b
  IAnd typ a b -> binOp "and" typ a b
  IOr typ a b -> binOp "or" typ a b
  IXOr typ a b -> binOp "xor" typ a b
  IShl typ a b -> binOp "shl" typ a b
  ILShr typ a b -> binOp "lshr" typ a b
  IAShr typ a b -> binOp "ashr" typ a b
  IFAdd typ a b -> binOp "fadd" typ a b
  IFSub typ a b -> binOp "fsub" typ a b
  IFMul typ a b -> binOp "fmul" typ a b
  IFDiv typ a b -> binOp "fdiv" typ a b
  IFNeg typ a ->
    "fneg " <> renderType typ <> " " <> renderOperand a
  IICmp cond typ a b ->
    "icmp " <> renderICmpCond cond <> " " <> renderType typ <> " " <> renderOperand a <> ", " <> renderOperand b
  IFCmp cond typ a b ->
    "fcmp " <> renderFCmpCond cond <> " " <> renderType typ <> " " <> renderOperand a <> ", " <> renderOperand b
  ILoad typ ptr ->
    "load " <> renderType typ <> ", ptr " <> renderOperand ptr
  IStore val ptr ->
    "store " <> renderTypedOperand val <> ", ptr " <> renderOperand ptr
  IAlloca typ n ->
    "alloca " <> renderType typ <> ", " <> renderTypedOperand n
  IGep typ base idxs ->
    "getelementptr "
      <> renderType typ
      <> ", ptr "
      <> renderOperand base
      <> foldMap (", " <>) (map renderTypedOperand idxs)
  IBitcast v typ ->
    "bitcast " <> renderOperand v <> " to " <> renderType typ
  ISext v typ ->
    "sext " <> renderTypedOperand v <> " to " <> renderType typ
  IZext v typ ->
    "zext " <> renderTypedOperand v <> " to " <> renderType typ
  ITrunc v typ ->
    "trunc " <> renderTypedOperand v <> " to " <> renderType typ
  IInttoptr v typ ->
    "inttoptr " <> renderTypedOperand v <> " to " <> renderType typ
  IPtrtoint v typ ->
    "ptrtoint " <> renderTypedOperand v <> " to " <> renderType typ
  ICall marker retTy fn args ->
    callPrefix
      <> renderType retTy
      <> " "
      <> renderOperand fn
      <> "("
      <> Text.intercalate ", " (map renderTypedOperand args)
      <> ")"
   where
    callPrefix = case marker of
      NoTail -> "call "
      Tail -> "tail call "
      MustTail -> "musttail call "
  IPhi typ incoming ->
    "phi "
      <> renderType typ
      <> " "
      <> Text.intercalate
        ", "
        ["[ " <> renderOperand op <> ", %" <> quoteIfNeeded blockName <> " ]" | (op, blockName) <- incoming]
  ISelect typ cond t f ->
    "select i1 "
      <> renderOperand cond
      <> ", "
      <> renderType typ
      <> " "
      <> renderOperand t
      <> ", "
      <> renderType typ
      <> " "
      <> renderOperand f
 where
  binOp mnemonic typ a b =
    mnemonic <> " " <> renderType typ <> " " <> renderOperand a <> ", " <> renderOperand b

-- | Render a terminator instruction.
renderTerminator :: IRTerminator -> Text
renderTerminator =
  \case
    IRet Nothing ->
      "ret void"
    IRet (Just op) ->
      "ret " <> renderTypedOperand op
    IBr target ->
      "br label %" <> quoteIfNeeded target
    ICondBr cond t f ->
      "br i1 "
        <> renderOperand cond
        <> ", label %"
        <> quoteIfNeeded t
        <> ", label %"
        <> quoteIfNeeded f
    ISwitch val default_ cases ->
      "switch "
        <> renderTypedOperand val
        <> ", label %"
        <> quoteIfNeeded default_
        <> " [    \n"
        <> Text.unlines
          [ "    "
            <> renderType (constantType caseVal)
            <> " "
            <> renderConstant caseVal
            <> ", label %"
            <> quoteIfNeeded caseTarget
          | (caseVal, caseTarget) <- cases
          ]
        <> "  ]"
    IUnreachable ->
      "unreachable"

-- | Render an operand without its type.
renderOperand :: IROperand -> Text
renderOperand =
  \case
    OLocal _ name -> "%" <> quoteIfNeeded name
    OGlobal _ name -> "@" <> quoteIfNeeded name
    OConstant c -> renderConstant c

-- | Render an operand prefixed with its type.
renderTypedOperand :: IROperand -> Text
renderTypedOperand op = renderType (operandType op) <> " " <> renderOperand op

-- | Render a type to its LLVM IR text representation.
renderType :: IRType -> Text
renderType =
  \case
    TInt n -> "i" <> Text.pack (show n)
    TFloat -> "float"
    TDouble -> "double"
    TVoid -> "void"
    TPtr -> "ptr"
    TStruct ts -> "{ " <> Text.intercalate ", " (map renderType ts) <> " }"
    TArray n t -> "[" <> Text.pack (show n) <> " x " <> renderType t <> "]"
    TFun retTy argTys ->
      renderType retTy <> " (" <> Text.intercalate ", " (map renderType argTys) <> ")"
    TNamed name -> "%" <> quoteIfNeeded name
    TOpaque name -> "opaque %" <> quoteIfNeeded name
    TVector n t -> "<" <> Text.pack (show n) <> " x " <> renderType t <> ">"

-- | Render a constant value.
renderConstant :: IRConstant -> Text
renderConstant =
  \case
    CInt _ i -> Text.pack (show i)
    CFloat f -> Text.pack (printf "0x%016X" (castDoubleToWord64 (realToFrac f :: Double)))
    CDouble d -> Text.pack (printf "0x%016X" (castDoubleToWord64 d))
    CNull _typ -> "null"
    CStruct cs -> "{ " <> Text.intercalate ", " (map renderConstant cs) <> " }"
    CArray _ cs -> "[ " <> Text.intercalate ", " (map renderConstant cs) <> " ]"

-- | Render typed function arguments.
renderFunctionArgs :: [(IRType, IRName)] -> Text
renderFunctionArgs args =
  Text.intercalate ", " [renderType typ <> " %" <> quoteIfNeeded name | (typ, name) <- args]

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

{- | Render a ByteString as an LLVM c"..." literal body, escaping non-printable
bytes as \XX hex escape sequences.
-}
renderByteStringLiteral :: ByteString -> Text
renderByteStringLiteral = Text.pack . concatMap renderByte . BS.unpack
 where
  renderByte b
    | b >= 32 && b <= 126 && b /= 34 && b /= 92 = [toEnum (fromIntegral b)]
    | otherwise = '\\' : [intToDigit (fromIntegral b `div` 16), intToDigit (fromIntegral b `mod` 16)]
