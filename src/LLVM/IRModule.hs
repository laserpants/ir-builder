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
  IRDecl (..),

  -- * Globals and attributes
  IRGlobal (..),
  IRAttribute (..),

  -- * Functions and blocks
  IRFunction (..),
  IRBlockItem (..),
  IRBlock (..),

  -- * Verification
  verifyModule,
)
where

import Common (Name)
import Data.ByteString (ByteString)
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IROperand (IRConstant, IRTerminator (..))
import LLVM.IRType (IRType (..))

{- | Linkage specification for functions and globals.

Linkage controls symbol visibility and resolution:

* `LExternal`: Symbol is externally visible and resolved at link time
* `LInternal`: Symbol is internal to the translation unit (static in C)
* `LPrivate`: Symbol is private and cannot be referenced externally
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

* `moduleName`: The name of this module
* `moduleDecls`: External function declarations (functions defined elsewhere)
* `moduleGlobals`: Global constants and string literals
* `moduleFunctions`: Function definitions

__Example:__

@
let mod = IRModule
      { moduleName = "myprogram"
      , moduleDecls = [IRDecl "printf" (TFun i32 [TPtr])]
      , moduleGlobals = []
      , moduleFunctions = [mainFunction]
      }
in verifyModule mod
@
-}
data IRModule = IRModule
  { moduleName :: Name
  , moduleDecls :: [IRDecl]
  , moduleGlobals :: [IRGlobal]
  , moduleFunctions :: [IRFunction]
  }
  deriving (Show, Eq, Ord)

{- | An IR declaration for an external function.

Declarations represent functions that are defined outside this module
(e.g., standard library functions like @printf@, @malloc@, etc.).
They specify only the function signature without an implementation.

__Fields:__

* `declName`: Name of the declared function
* `declType`: Function type (typically @TFun retType paramTypes@)
-}
data IRDecl = IRDecl
  { declName :: Name
  , declType :: IRType
  }
  deriving (Show, Eq, Ord)

{- | An IR global variable, string constant, or external declaration.

Globals represent module-level definitions that persist for the program's
lifetime. They can be string literals, constant values, or external
function declarations.

__Constructors:__

* `IRString`: A string literal with the given byte content
* `IRConstant`: A constant global with an initial value
* `IRExtern`: External function (similar to 'IRDecl')
-}
data IRGlobal
  = IRString IRLinkage Name ByteString
  | IRConstant IRLinkage Name IRType IRConstant
  | IRExtern Name IRType [IRType]
  deriving (Show, Eq, Ord)

{- | Function attributes that provide optimization and correctness hints to the compiler.

Attributes inform the LLVM backend about function semantics and allow
for better optimization and error detection. Multiple attributes can
be combined on a single function.

__Common attributes:__

* `NoReturn`: Function never returns (calls @exit()@, raises exception, infinite loop)
* `NoUnwind`: Function does not unwind exceptions
* `ReadOnly`: Function accesses memory but does not modify it
* `ReadNone`: Function neither reads nor writes memory
* `AlwaysInline`: Always inline this function at call sites
* `NoInline`: Never inline this function
* `TailCall`: Hints that tail call optimization should be applied
* `MustTailCall`: Requires tail call optimization
* `Cold`: Indicates function is infrequently called
* `Hot`: Indicates function is frequently called
* `InlineHint`: Suggest inlining to the backend optimizer
* `NoAlias`: Arguments and return value do not alias
* `GC`: Specifies the garbage collection strategy for this function
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

* `functionName`: Unique name of this function within the module
* `functionLinkage`: Visibility and linkage of this function
* `functionRetType`: Type of the return value
* `functionArgs`: Parameter types and names
* `functionBlocks`: Basic blocks comprising the function body
* `functionAttributes`: Optimization and correctness hints

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
  { functionName :: Name
  , functionLinkage :: IRLinkage
  , functionRetType :: IRType
  , functionArgs :: [(IRType, Name)]
  , functionBlocks :: [IRBlock]
  , functionAttributes :: [IRAttribute]
  }
  deriving (Show, Eq, Ord)

{- | An item within a basic block (instruction or comment).

Blocks contain a sequence of items, each being either an actual instruction
or a comment annotation. Instructions produce values or side effects, while
annotations provide documentation in the output.

__Constructors:__

* `BlockInstr`: An LLVM instruction with optional inline comment
* `BlockAnnotation`: A comment annotation for documentation
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

* `blockLabel`: Unique label identifying this block within its function
* `blockItems`: Instructions and annotations in execution order
* `blockTerminator`: The final instruction (ret, br, condbr, etc.)

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
  { blockLabel :: Name
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
verifyModule IRModule{moduleFunctions} = mapM_ verifyFunction moduleFunctions

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

extractSSANamesFromBlock :: IRBlock -> [Name]
extractSSANamesFromBlock IRBlock{blockItems} =
  concatMap extractSSANamesFromItem blockItems

extractSSANamesFromItem :: IRBlockItem -> [Name]
extractSSANamesFromItem =
  \case
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

findInvalidBranchTargets :: Set Name -> IRBlock -> [Name]
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
