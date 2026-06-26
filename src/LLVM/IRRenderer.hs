{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
This module provides functionality for rendering LLVM IR data structures into their
textual LLVM IR representation. It handles the conversion of IR modules, functions,
blocks, instructions, types, and operands into properly formatted LLVM assembly code.

The renderer uses a monadic transformer interface allowing it to run in any monad
context (pure, IO, or custom monad stacks). The primary entry point is 'renderModule',
which takes an 'IRModule' and produces the complete textual IR output.
-}
module LLVM.IRRenderer (
  IRRenderer,
  IRRendererT (..),
  runIRRenderer,
  runIRRendererT,
  renderModule,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit, isAlphaNum)
import Data.Functor.Identity (Identity (runIdentity))
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
  IRDecl (..),
  IRFunction (..),
  IRGlobal (..),
  IRLinkage (..),
  IRModule (..),
 )
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRName, IRType (..))
import Text.Printf (printf)

{- | Returns True if the name contains characters that require quoting in LLVM IR.
Safe (unquoted) identifiers match [-a-zA-Z$._][-a-zA-Z$._0-9]*.
-}
needsQuoting :: IRName -> Bool
needsQuoting n = case Text.uncons n of
  Nothing -> True
  Just (h, t) -> not (isSafeHead h) || Text.any (not . isSafeBody) t
 where
  isSafeHead c = isAlphaNum c || c == '-' || c == '$' || c == '.' || c == '_'
  isSafeBody c = isSafeHead c

{- | Wrap a name in double-quotes if it contains characters that LLVM IR
requires to be quoted; leave it bare otherwise.
-}
quoteIfNeeded :: IRName -> Text
quoteIfNeeded n
  | needsQuoting n = "\"" <> n <> "\""
  | otherwise = n

{- |
The IRRendererT monad transformer provides a context for rendering LLVM IR.

This newtype wraps any monad, allowing rendering operations to be composed in pure
code, IO, or any custom monad stack. It provides a clean abstraction for rendering
while allowing users to choose their execution context.

For pure rendering, use the 'IRRenderer' type alias with 'runIRRenderer'.
For rendering in other monads (IO, Either, etc.), use 'runIRRendererT'.
-}
newtype IRRendererT m a = IRRendererT {unpackIRRendererT :: m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    )

{- |
Type alias for pure rendering computations.

This is the most common use case for rendering LLVM IR in a pure context.
-}
type IRRenderer = IRRendererT Identity

{- |
Execute an IRRendererT computation in any monad.

This function unwraps the renderer transformer and returns the underlying
monadic computation.

==== __Example__

@
-- Pure rendering
result :: Text
result = runIdentity $ runIRRendererT $ renderModule myModule

-- Rendering with IO
resultIO :: IO Text
resultIO = runIRRendererT $ renderModule myModule
@
-}
runIRRendererT :: IRRendererT m a -> m a
runIRRendererT = unpackIRRendererT

{- |
Execute a pure IRRenderer computation and extract the result.

This is a convenience function for the common case of pure rendering.

==== __Example__

@
result = runIRRenderer $ renderModule myModule
@
-}
runIRRenderer :: IRRenderer a -> a
runIRRenderer = runIdentity . runIRRendererT

{- |
Render an LLVM IR module to its textual representation.

This is the primary entry point for rendering complete LLVM IR modules. It processes
all module components in order:

1. Type declarations (structs, named types)
2. Global variables and constants
3. Function definitions

The output is well-formed LLVM IR that can be written to a .ll file or passed to
LLVM tools like llc or opt.

==== __Parameters__

* 'IRModule' - The module containing declarations, globals, and functions to render

==== __Returns__

A 'Text' value containing the complete LLVM IR representation of the module,
with proper formatting and newline separation between top-level definitions.
-}
renderModule :: (Monad m) => IRModule -> IRRendererT m Text
renderModule IRModule{moduleDecls, moduleGlobals, moduleFunctions} = do
  decls <- traverse renderDecl moduleDecls
  globs <- traverse renderGlobal moduleGlobals
  funs <- traverse renderFunction moduleFunctions
  let items = concat [decls, globs, funs]
  pure $ if null items then "" else Text.intercalate "\n" items <> "\n"

-- | Render a type declaration (e.g., "%Type = type { i32, i32 }")
renderDecl :: (Monad m) => IRDecl -> IRRendererT m Text
renderDecl IRDecl{declName, declType} = do
  typeStr <- renderType declType
  pure $ "%" <> quoteIfNeeded declName <> " = type " <> typeStr <> "\n"

-- | Render a global variable or external declaration
renderGlobal :: (Monad m) => IRGlobal -> IRRendererT m Text
renderGlobal =
  \case
    IRString linkage name bs ->
      let linkageStr = renderLinkage linkage
          len = BS.length bs
          content = renderByteStringLiteral bs
       in pure $ "@" <> quoteIfNeeded name <> " = " <> linkageStr <> "constant [" <> Text.pack (show len) <> " x i8] c\"" <> content <> "\"\n"
    IRConstant linkage name typ val -> do
      typeStr <- renderType typ
      valStr <- renderConstant val
      let linkageStr = renderLinkage linkage
      pure $ "@" <> quoteIfNeeded name <> " = " <> linkageStr <> " constant " <> typeStr <> " " <> valStr <> "\n"
    IRVar linkage name typ val -> do
      typeStr <- renderType typ
      valStr <- renderConstant val
      let linkageStr = renderLinkage linkage
      pure $ "@" <> quoteIfNeeded name <> " = " <> linkageStr <> "global " <> typeStr <> " " <> valStr <> "\n"
    IRExtern name retTy argTys -> do
      retTyStr <- renderType retTy
      argTyStrs <- mapM renderType argTys
      pure $ "declare " <> retTyStr <> " @" <> quoteIfNeeded name <> "(" <> Text.intercalate ", " argTyStrs <> ")\n"

-- | Render a function definition
renderFunction :: (Monad m) => IRFunction -> IRRendererT m Text
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
      <> quoteIfNeeded functionName
      <> "("
      <> argsStr
      <> ")"
      <> attrsStr
      <> " {\n"
      <> Text.unlines blocksStr
      <> "}\n"

-- | Render a single block
renderBlock :: (Monad m) => IRBlock -> IRRendererT m Text
renderBlock IRBlock{blockLabel, blockItems, blockTerminator} = do
  itemsStrs <- mapM renderBlockItem blockItems
  termStr <- renderTerminator blockTerminator
  pure $
    quoteIfNeeded blockLabel
      <> ":\n"
      <> Text.unlines itemsStrs
      <> "  "
      <> termStr

-- | Render a block item (instruction or annotation)
renderBlockItem :: (Monad m) => IRBlockItem -> IRRendererT m Text
renderBlockItem =
  \case
    BlockInstr instr ->
      renderInstruction instr
    BlockAnnotation ann ->
      case ann of
        Comment txt -> pure $ "  ; " <> txt
        CommentBlock txts -> pure $ Text.unlines [("  ; " <>) line | line <- txts]

-- | Render an instruction
renderInstruction :: (Monad m) => IRInstruction (Maybe Text) -> IRRendererT m Text
renderInstruction IRInstruction{instrResult, instrOp, instrMetadata} = do
  opStr <- renderInstrOp instrOp
  let baseStr = case instrResult of
        Nothing ->
          "  " <> opStr
        Just (name, _typ) ->
          "  %" <> quoteIfNeeded name <> " = " <> opStr
  case instrMetadata of
    Nothing ->
      pure baseStr
    Just comment ->
      pure $ baseStr <> "  ; " <> comment

-- | Render the instruction operation
renderInstrOp :: (Monad m) => IRInstrOp -> IRRendererT m Text
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
    ILoad typ ptr -> do
      tyStr <- renderType typ
      ptrStr <- renderOperand ptr
      pure $ "load " <> tyStr <> ", ptr " <> ptrStr
    IStore val ptr -> do
      valStr <- renderTypedOperand val
      ptrStr <- renderOperand ptr
      pure $ "store " <> valStr <> ", ptr " <> ptrStr
    IAlloca typ n -> do
      tyStr <- renderType typ
      nStr <- renderOperand n
      pure $ "alloca " <> tyStr <> ", i32 " <> nStr
    IGep typ base idxs -> do
      tyStr <- renderType typ
      baseStr <- renderOperand base
      idxStrs <- mapM renderTypedOperand idxs
      pure $ "getelementptr " <> tyStr <> ", ptr " <> baseStr <> foldMap (", " <>) idxStrs
    IBitcast v typ -> do
      vStr <- renderOperand v
      tyStr <- renderType typ
      pure $ "bitcast " <> vStr <> " to " <> tyStr
    ISext v typ -> do
      vStr <- renderTypedOperand v
      tyStr <- renderType typ
      pure $ "sext " <> vStr <> " to " <> tyStr
    IZext v typ -> do
      vStr <- renderTypedOperand v
      tyStr <- renderType typ
      pure $ "zext " <> vStr <> " to " <> tyStr
    ITrunc v typ -> do
      vStr <- renderTypedOperand v
      tyStr <- renderType typ
      pure $ "trunc " <> vStr <> " to " <> tyStr
    IInttoptr v typ -> do
      vStr <- renderTypedOperand v
      tyStr <- renderType typ
      pure $ "inttoptr " <> vStr <> " to " <> tyStr
    IPtrtoint v typ -> do
      vStr <- renderTypedOperand v
      tyStr <- renderType typ
      pure $ "ptrtoint " <> vStr <> " to " <> tyStr
    ICall marker retTy fn args -> do
      retTyStr <- renderType retTy
      fnStr <- renderOperand fn
      argsStrs <- mapM renderTypedOperand args
      let callStr = callPrefix marker <> retTyStr <> " " <> fnStr <> "(" <> Text.intercalate ", " argsStrs <> ")"
      pure callStr
     where
      callPrefix =
        \case
          NoTail -> "call "
          Tail -> "tail call "
          MustTail -> "musttail call "
    IPhi typ incoming -> do
      tyStr <- renderType typ
      incomingStrs <- mapM renderPhiIncoming incoming
      pure $ "phi " <> tyStr <> " " <> Text.intercalate ", " (map (\s -> "[ " <> s <> " ]") incomingStrs)
     where
      renderPhiIncoming (op, blockName) = do
        opStr <- renderOperand op
        pure $ opStr <> ", %" <> quoteIfNeeded blockName
    ISelect typ cond t f -> do
      tyStr <- renderType typ
      condStr <- renderOperand cond
      tStr <- renderOperand t
      fStr <- renderOperand f
      pure $ "select i1 " <> condStr <> ", " <> tyStr <> " " <> tStr <> ", " <> tyStr <> " " <> fStr

-- | Render a terminator instruction
renderTerminator :: (Monad m) => IRTerminator -> IRRendererT m Text
renderTerminator =
  \case
    IRet Nothing ->
      pure "ret void"
    IRet (Just op) -> do
      opStr <- renderTypedOperand op
      pure $ "ret " <> opStr
    IBr target ->
      pure $ "br label %" <> quoteIfNeeded target
    ICondBr cond t f -> do
      condStr <- renderOperand cond
      pure $ "br i1 " <> condStr <> ", label %" <> quoteIfNeeded t <> ", label %" <> quoteIfNeeded f
    ISwitch val default_ cases -> do
      valStr <- renderTypedOperand val
      casesStrs <- mapM renderSwitchCase cases
      pure $ "switch " <> valStr <> ", label %" <> quoteIfNeeded default_ <> " [    \n" <> Text.unlines casesStrs <> "  ]"
     where
      renderSwitchCase (caseVal, caseTarget) = do
        tyStr <- renderType (constantType caseVal)
        caseValStr <- renderConstant caseVal
        pure $ "    " <> tyStr <> " " <> caseValStr <> ", label %" <> quoteIfNeeded caseTarget
    IUnreachable ->
      pure "unreachable"

-- | Render an operand without its type (used where the type is already stated by the op)
renderOperand :: (Monad m) => IROperand -> IRRendererT m Text
renderOperand =
  \case
    OLocal _ name ->
      pure $ "%" <> quoteIfNeeded name
    OGlobal _ name ->
      pure $ "@" <> quoteIfNeeded name
    OConstant c ->
      renderConstant c

-- | Render an operand prefixed with its type (used in ret, call args, store value)
renderTypedOperand :: (Monad m) => IROperand -> IRRendererT m Text
renderTypedOperand op = do
  tyStr <- renderType (operandType op)
  opStr <- renderOperand op
  pure $ tyStr <> " " <> opStr

-- | Derive the type of an operand
operandType :: IROperand -> IRType
operandType =
  \case
    OLocal t _ -> t
    OGlobal t _ -> t
    OConstant c -> constantType c

-- | Derive the type of a constant
constantType :: IRConstant -> IRType
constantType =
  \case
    CInt n _ ->
      TInt n
    CFloat _ ->
      TFloat
    CDouble _ ->
      TDouble
    CNull t ->
      t
    CStruct cs ->
      TStruct (map constantType cs)
    CArray t _ ->
      t

-- | Render a type
renderType :: (Monad m) => IRType -> IRRendererT m Text
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
    TPtr ->
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
    TNamed name -> pure $ "%" <> quoteIfNeeded name
    TOpaque name -> pure $ "opaque %" <> quoteIfNeeded name
    TVector n t -> do
      tStr <- renderType t
      pure $ "<" <> Text.pack (show n) <> " x " <> tStr <> ">"

-- | Render a constant value
renderConstant :: (Monad m) => IRConstant -> IRRendererT m Text
renderConstant =
  \case
    CInt _ i ->
      pure $ Text.pack (show i)
    CFloat f ->
      pure $ Text.pack (printf "0x%016X" (castDoubleToWord64 (realToFrac f :: Double)))
    CDouble d ->
      pure $ Text.pack (printf "0x%016X" (castDoubleToWord64 d))
    CNull _typ ->
      pure "null"
    CStruct cs -> do
      csStrs <- mapM renderConstant cs
      pure $ "{ " <> Text.intercalate ", " csStrs <> " }"
    CArray _ cs -> do
      csStrs <- mapM renderConstant cs
      pure $ "[ " <> Text.intercalate ", " csStrs <> " ]"

-- | Render function arguments
renderFunctionArgs :: (Monad m) => [(IRType, IRName)] -> IRRendererT m Text
renderFunctionArgs args = do
  argStrs <- mapM (\(typ, name) -> do tyStr <- renderType typ; pure $ tyStr <> " %" <> quoteIfNeeded name) args
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

{- | Render a ByteString as an LLVM c"..." literal body, escaping non-printable
bytes as \XX hex escape sequences.
-}
renderByteStringLiteral :: ByteString -> Text
renderByteStringLiteral = Text.pack . concatMap renderByte . BS.unpack
 where
  renderByte b
    | b >= 32 && b <= 126 && b /= 34 && b /= 92 = [toEnum (fromIntegral b)]
    | otherwise = '\\' : [intToDigit (fromIntegral b `div` 16), intToDigit (fromIntegral b `mod` 16)]
