{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- | This module defines the fundamental data structures that represent compiled
LLVM IR (Intermediate Representation). It is the output produced by the
'LLVM.IRBuilder' DSL and serves as the canonical representation of an IR module.

The module provides:

* Data types for modules, functions, blocks, and instructions
* Linkage specifications for visibility and symbol resolution
* Function attributes for optimization hints and constraints
* Comprehensive verification to detect structural errors

= Example: Inspecting an IR module

Once compiled using 'LLVM.IRBuilder.buildModule', a module can be verified
and rendered:

@
let module = buildModule "my_module" $ define i32 "main" [] LExternal [] $ do
 b0 <- beginBlock "entry"
 ret (int32 42)
case verifyModule module of
 Left err -> putStrLn $ "Verification failed: " ++ err
 Right () -> putStrLn $ "Module verified successfully"
@
-}
module LLVM.IRModule (
  -- * Core types
  IRLinkage (..),
  IRModule (..),
  IRTypeDecl (..),

  -- * Globals and attributes
  IRGlobal (..),
  IRAttribute (..),

  -- * Functions and blocks
  IRFunction (..),
  IRBlockItem (..),
  IRBlock (..),

  -- * Verification
  verifyModule,
  typeCheckModule,
)
where

import Data.ByteString (ByteString)
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
import LLVM.IROperand (IRConstant, IRTerminator (..), operandType)
import LLVM.IRType (IRName, IRType (..))

{- | Linkage specification for functions and globals.

Linkage controls symbol visibility and resolution:

* 'LExternal': symbol is externally visible and resolved at link time
* 'LInternal': symbol is internal to the translation unit (like @static@ in C)
* 'LPrivate': symbol is private and cannot be referenced externally
-}
data IRLinkage
  = LExternal
  | LInternal
  | LPrivate
  deriving (Show, Eq, Ord)

{- | An LLVM IR module is the top-level compilation unit.

It contains all declarations, global definitions, and function definitions
for a program. Modules can be verified for structural correctness and
rendered to LLVM assembly text.

__Fields:__

* 'irModuleName': the name of this module
* 'irModuleTypeDecls': named type declarations (e.g. @%Node = type { ... }@)
* 'irModuleGlobals': global constants and string literals
* 'irModuleFunctions': function definitions

__Example:__

@
let mod = IRModule
 { irModuleName = "myprogram"
 , irModuleTypeDecls = [IRTypeDecl "Node" (TStruct [TInt 32, TPtr])]
 , irModuleGlobals = []
 , irModuleFunctions = [mainFunction]
 }
in verifyModule mod
@
-}
data IRModule = IRModule
  { irModuleName :: IRName
  , irModuleTypeDecls :: [IRTypeDecl]
  , irModuleGlobals :: [IRGlobal]
  , irModuleFunctions :: [IRFunction]
  }
  deriving (Show, Eq, Ord)

{- | A named type declaration.

Type declarations define named types in the module, rendered as:

@
%Node = type { i32, ptr, ptr }
@

__Fields:__

* 'typeDeclName': the name of the declared type
* 'typeDeclType': the underlying type definition
-}
data IRTypeDecl = IRTypeDecl
  { typeDeclName :: IRName
  , typeDeclType :: IRType
  }
  deriving (Show, Eq, Ord)

{- | An IR global variable, string constant, or external declaration.

Globals represent module-level definitions that persist for the program's
lifetime. They can be string literals, constant values, mutable global
variables, or external function declarations.

__Constructors:__

* 'IRString': a string literal with the given byte content
* 'IRConstant': an immutable (constant) global with an initial value
* 'IRVar': a mutable global variable with an initial value
* 'IRExtern': an external function declaration
-}
data IRGlobal
  = IRString IRLinkage IRName ByteString
  | IRConstant IRLinkage IRName IRType IRConstant
  | IRVar IRLinkage IRName IRType IRConstant
  | -- | @IRExtern name retType argTypes isVariadic@.
    -- Set @isVariadic = True@ to emit @declare retType \@name(argTypes, ...)@.
    IRExtern IRName IRType [IRType] Bool
  deriving (Show, Eq, Ord)

{- | Function attributes that provide optimization and correctness hints to the compiler.

Attributes inform the LLVM backend about function semantics and allow
for better optimization and error detection. Multiple attributes can
be combined on a single function.

__Common attributes:__

* 'NoReturn': function never returns (e.g., calls @exit()@, raises an exception, or loops forever)
* 'NoUnwind': function does not unwind the stack
* 'ReadOnly': function accesses memory but does not modify it
* 'ReadNone': function neither reads nor writes memory
* 'AlwaysInline': always inline this function at call sites
* 'NoInline': never inline this function
* 'TailCall': hints that tail-call optimization should be applied
* 'MustTailCall': requires tail-call optimization
* 'Cold': indicates this function is infrequently called
* 'Hot': indicates this function is frequently called
* 'InlineHint': suggests inlining to the backend optimizer
* 'NoAlias': arguments and return value do not alias
* 'GC': specifies the garbage-collection strategy for this function
-}
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

{- | An IR function definition.

A function consists of a signature, body (basic blocks), and optional attributes.
The body is organized as a control flow graph with blocks connected by
branch terminators. All paths through the function must end with a terminator
(return, branch, or unreachable).

__Fields:__

* 'functionName': unique name of this function within the module
* 'functionLinkage': visibility and linkage
* 'functionRetType': return type
* 'functionArgs': parameter types and names
* 'functionBlocks': basic blocks comprising the function body
* 'functionAttributes': optimization and correctness hints

__Example:__

@
let func = IRFunction
 { functionName = "main"
 , functionLinkage = LExternal
 , functionRetType = i32
 , functionArgs = []
 , functionBlocks = [entryBlock, loopBlock, exitBlock]
 , functionAttributes = []
 }
in verifyFunction func
@
-}
data IRFunction = IRFunction
  { functionName :: IRName
  , functionLinkage :: IRLinkage
  , functionRetType :: IRType
  , functionArgs :: [(IRType, IRName)]
  , functionBlocks :: [IRBlock]
  , functionAttributes :: [IRAttribute]
  }
  deriving (Show, Eq, Ord)

{- | An item within a basic block (instruction or comment).

Blocks contain a sequence of items, each being either an actual instruction
or a comment annotation. Instructions produce values or side effects, while
annotations provide documentation in the output.

__Constructors:__

* 'BlockInstr': an LLVM instruction with an optional inline comment
* 'BlockAnnotation': a comment annotation for documentation
-}
data IRBlockItem
  = BlockInstr (IRInstruction (Maybe Text))
  | BlockAnnotation IRAnnotation
  deriving (Show, Eq, Ord)

{- | A basic block in LLVM IR.

Basic blocks are maximal sequences of instructions with:

* A single entry point (the label)
* No control flow within the block
* A single exit point (the terminator)

Control flow between blocks is managed through branch and conditional
branch terminators. All paths through a block must end with exactly one
terminator instruction.

__Fields:__

* 'blockLabel': unique label identifying this block within its function
* 'blockItems': instructions and annotations in execution order
* 'blockTerminator': the terminating instruction (@ret@, @br@, @condbr@, etc.)

__Example:__

@
let block = IRBlock
 { blockLabel = "entry"
 , blockItems = [BlockInstr (ICall i32 "printf" [...])]
 , blockTerminator = IRet (Just (int32 0))
 }
in blockLabel block -- "entry"
@
-}
data IRBlock = IRBlock
  { blockLabel :: IRName
  , blockItems :: [IRBlockItem]
  , blockTerminator :: IRTerminator
  }
  deriving (Show, Eq, Ord)

{- | Verify the structural correctness of an IR module.

Verification checks for:

* No duplicate block labels within functions
* No duplicate SSA value names within functions
* All branch targets refer to existing blocks

Returns @Left errorMsg@ if verification fails, or @Right ()@ if successful.
This function is useful for detecting errors in IR generation before
rendering to assembly or executing.

__Example:__

@
let mod = buildModule "test" $ do
 define i32 "main" [] LExternal [] $ do
   b0 <- beginBlock "entry"
   ret (int32 42)
case verifyModule mod of
 Left err -> putStrLn $ "Error: " ++ err
 Right () -> putStrLn "Module is valid"
@
-}
verifyModule :: IRModule -> Either String ()
verifyModule IRModule{irModuleFunctions} = mapM_ verifyFunction irModuleFunctions

verifyFunction :: IRFunction -> Either String ()
verifyFunction f = do
  verifyNoDuplicateBlockNames f
  verifyNoDuplicateSSANames f
  verifyBranchTargetsExist f

verifyNoDuplicateBlockNames :: IRFunction -> Either String ()
verifyNoDuplicateBlockNames IRFunction{functionName, functionBlocks} =
  if null duplicates
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has duplicate block names: "
          ++ show (map Text.unpack duplicates)
 where
  labels = map blockLabel functionBlocks
  duplicates = labels \\ nub labels

verifyNoDuplicateSSANames :: IRFunction -> Either String ()
verifyNoDuplicateSSANames IRFunction{functionName, functionBlocks} =
  if null duplicates
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has duplicate SSA names: "
          ++ show (map Text.unpack duplicates)
 where
  ssaNames = concatMap extractSSANamesFromBlock functionBlocks
  duplicates = ssaNames \\ nub ssaNames

extractSSANamesFromBlock :: IRBlock -> [IRName]
extractSSANamesFromBlock IRBlock{blockItems} =
  concatMap extractSSANamesFromItem blockItems

extractSSANamesFromItem :: IRBlockItem -> [IRName]
extractSSANamesFromItem =
  \case
    BlockInstr IRInstruction{instrResult = Just (name, _)} -> [name]
    BlockInstr _ -> []
    BlockAnnotation _ -> []

verifyBranchTargetsExist :: IRFunction -> Either String ()
verifyBranchTargetsExist IRFunction{functionName, functionBlocks} =
  if null invalidTargets
    then Right ()
    else
      Left $
        "Function "
          ++ Text.unpack functionName
          ++ " has invalid branch targets: "
          ++ show (map Text.unpack invalidTargets)
 where
  validLabels = Set.fromList (map blockLabel functionBlocks)
  invalidTargets = concatMap (findInvalidBranchTargets validLabels) functionBlocks

findInvalidBranchTargets :: Set IRName -> IRBlock -> [IRName]
findInvalidBranchTargets validLabels IRBlock{blockTerminator} =
  case blockTerminator of
    IBr target ->
      [target | target `Set.notMember` validLabels]
    ICondBr _ t1 t2 ->
      [target | target <- [t1, t2], target `Set.notMember` validLabels]
    ISwitch _ default_ cases ->
      let targetLabels = default_ : map snd cases
       in [target | target <- targetLabels, target `Set.notMember` validLabels]
    _ ->
      []

-- ============================================================================
-- Type checking
-- ============================================================================

{- | Type-check all instructions in an IR module.

Validates that operand types are consistent with the declared instruction type.
Returns @Left errorMsg@ with a descriptive message on the first type mismatch,
or @Right ()@ if all instructions are well-typed.

This is a post-hoc validation pass — call it after 'LLVM.IRBuilder.buildModule'
to catch type errors before rendering.

__Checks performed:__

* Binary arithmetic/bitwise operands match the declared type
* Comparison operands match the declared type; result is @i1@
* @select@ condition is @i1@; branch values match the declared type
* @phi@ incoming operand types match the declared type
* @load@/@store@ pointer operand is @ptr@
* @alloca@ count operand is an integer type
* @sext@/@zext@/@trunc@ width constraints
* @inttoptr@/@ptrtoint@ integer/pointer constraints
* @getelementptr@ base is @ptr@; index operands are integers

__Not checked:__

* @call@ argument types (requires resolving callee type)
* @bitcast@ size compatibility (requires data layout)
-}
typeCheckModule :: IRModule -> Either String ()
typeCheckModule IRModule{irModuleFunctions} = mapM_ typeCheckFunction irModuleFunctions

typeCheckFunction :: IRFunction -> Either String ()
typeCheckFunction IRFunction{functionName, functionBlocks} =
  mapM_ (typeCheckBlock functionName) functionBlocks

typeCheckBlock :: IRName -> IRBlock -> Either String ()
typeCheckBlock funcName IRBlock{blockLabel, blockItems} =
  mapM_ (typeCheckItem funcName blockLabel) blockItems

typeCheckItem :: IRName -> IRName -> IRBlockItem -> Either String ()
typeCheckItem funcName blockLabel' = \case
  BlockInstr IRInstruction{instrOp, instrResult} ->
    typeCheckInstrOp funcName blockLabel' instrResult instrOp
  BlockAnnotation _ -> Right ()

typeCheckInstrOp :: IRName -> IRName -> Maybe (IRName, IRType) -> IRInstrOp -> Either String ()
typeCheckInstrOp fn bl res = \case
  IAdd t a b -> checkBin "add" t a b
  ISub t a b -> checkBin "sub" t a b
  IMul t a b -> checkBin "mul" t a b
  IUDiv t a b -> checkBin "udiv" t a b
  ISDiv t a b -> checkBin "sdiv" t a b
  IURem t a b -> checkBin "urem" t a b
  ISRem t a b -> checkBin "srem" t a b
  IAShr t a b -> checkBin "ashr" t a b
  ILShr t a b -> checkBin "lshr" t a b
  IShl t a b -> checkBin "shl" t a b
  IAnd t a b -> checkBin "and" t a b
  IOr t a b -> checkBin "or" t a b
  IXOr t a b -> checkBin "xor" t a b
  IFAdd t a b -> checkBin "fadd" t a b
  IFSub t a b -> checkBin "fsub" t a b
  IFMul t a b -> checkBin "fmul" t a b
  IFDiv t a b -> checkBin "fdiv" t a b
  IFNeg t a -> do
    checkOp "fneg" "operand" t a
    checkRes "fneg" t
  IICmp _ t a b -> do
    checkOp "icmp" "lhs" t a
    checkOp "icmp" "rhs" t b
    checkRes "icmp" (TInt 1)
  IFCmp _ t a b -> do
    checkOp "fcmp" "lhs" t a
    checkOp "fcmp" "rhs" t b
    checkRes "fcmp" (TInt 1)
  ISelect t cond a b -> do
    checkOp "select" "condition" (TInt 1) cond
    checkOp "select" "true-value" t a
    checkOp "select" "false-value" t b
    checkRes "select" t
  IPhi t incoming -> do
    mapM_ (\(v, _) -> checkOp "phi" "incoming" t v) incoming
    checkRes "phi" t
  ILoad t ptr -> do
    checkOp "load" "pointer" TPtr ptr
    checkRes "load" t
  IStore _ ptr ->
    checkOp "store" "pointer" TPtr ptr
  IAlloca _ count -> do
    checkIsInt "alloca" "count" count
    checkRes "alloca" TPtr
  ISext src dst -> case (operandType src, dst) of
    (TInt w1, TInt w2)
      | w2 > w1 -> checkRes "sext" dst
      | otherwise ->
          Left $
            ctx
              ++ "sext: destination width ("
              ++ show w2
              ++ ") must be greater than source width ("
              ++ show w1
              ++ ")"
    (TInt _, _) ->
      Left $ ctx ++ "sext: destination must be an integer type, got " ++ show dst
    (srcTy, _) ->
      Left $ ctx ++ "sext: source must be an integer type, got " ++ show srcTy
  IZext src dst -> case (operandType src, dst) of
    (TInt w1, TInt w2)
      | w2 > w1 -> checkRes "zext" dst
      | otherwise ->
          Left $
            ctx
              ++ "zext: destination width ("
              ++ show w2
              ++ ") must be greater than source width ("
              ++ show w1
              ++ ")"
    (TInt _, _) ->
      Left $ ctx ++ "zext: destination must be an integer type, got " ++ show dst
    (srcTy, _) ->
      Left $ ctx ++ "zext: source must be an integer type, got " ++ show srcTy
  ITrunc src dst -> case (operandType src, dst) of
    (TInt w1, TInt w2)
      | w2 < w1 -> checkRes "trunc" dst
      | otherwise ->
          Left $
            ctx
              ++ "trunc: destination width ("
              ++ show w2
              ++ ") must be less than source width ("
              ++ show w1
              ++ ")"
    (TInt _, _) ->
      Left $ ctx ++ "trunc: destination must be an integer type, got " ++ show dst
    (srcTy, _) ->
      Left $ ctx ++ "trunc: source must be an integer type, got " ++ show srcTy
  IInttoptr src dst -> do
    checkIsInt "inttoptr" "source" src
    checkTy "inttoptr" "destination" TPtr dst
    checkRes "inttoptr" TPtr
  IPtrtoint src dst -> do
    checkOp "ptrtoint" "source" TPtr src
    checkIsIntTy "ptrtoint" "destination" dst
    checkRes "ptrtoint" dst
  IGep _ base idxs -> do
    checkOp "getelementptr" "base" TPtr base
    mapM_ (checkIsInt "getelementptr" "index") idxs
  IBitcast _ dst ->
    checkRes "bitcast" dst
  ICall{} ->
    Right ()
  IExtractValue{} ->
    Right ()
  IInsertValue{} ->
    Right ()
  IExtractElement vec idx ->
    checkIsInt "extractelement" "index" idx >> checkOp "extractelement" "vec" (operandType vec) vec
  IInsertElement vec elt idx -> do
    checkIsInt "insertelement" "index" idx
    checkOp "insertelement" "element" (operandType elt) elt
    checkOp "insertelement" "vec" (operandType vec) vec
  IShuffleVector _ _ _ ->
    Right ()
  IAtomicRMW _ _ ptr val ->
    checkOp "atomicrmw" "pointer" TPtr ptr >> checkRes "atomicrmw" (operandType val)
  ICmpXchg _ _ _ ptr cmp new_ ->
    checkOp "cmpxchg" "pointer" TPtr ptr
      >> checkOp "cmpxchg" "new" (operandType cmp) new_
      >> checkRes "cmpxchg" (TStruct [operandType new_, TInt 1])
  IFence{} ->
    Right ()
  IFreeze op ->
    checkRes "freeze" (operandType op)
 where
  ctx = "In function '" ++ Text.unpack fn ++ "', block '" ++ Text.unpack bl ++ "': "

  checkBin name t a b = do
    checkOp name "lhs" t a
    checkOp name "rhs" t b
    checkRes name t

  checkOp name role expected actual =
    let got = operandType actual
     in if got == expected
          then Right ()
          else
            Left $
              ctx
                ++ name
                ++ " "
                ++ role
                ++ " type mismatch — expected "
                ++ show expected
                ++ ", got "
                ++ show got

  checkIsInt name role op_ =
    case operandType op_ of
      TInt _ -> Right ()
      got ->
        Left $ ctx ++ name ++ " " ++ role ++ " must be an integer type, got " ++ show got

  checkRes name expected =
    case res of
      Nothing -> Right ()
      Just (_, got) ->
        if got == expected
          then Right ()
          else
            Left $
              ctx
                ++ name
                ++ " result type mismatch — expected "
                ++ show expected
                ++ ", got "
                ++ show got

  checkTy name role expected actual =
    if actual == expected
      then Right ()
      else
        Left $
          ctx
            ++ name
            ++ " "
            ++ role
            ++ " type mismatch — expected "
            ++ show expected
            ++ ", got "
            ++ show actual

  checkIsIntTy name role t =
    case t of
      TInt _ -> Right ()
      _ ->
        Left $ ctx ++ name ++ " " ++ role ++ " must be an integer type, got " ++ show t
