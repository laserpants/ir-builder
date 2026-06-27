{- | Main entry point for the @coal-llvm-subsystem@ library.

A single import provides the complete public API for constructing, verifying,
and rendering LLVM IR:

@
import LLVM.IR
@

This module re-exports the builder DSL, all instruction and type constructors,
the IR data types, and the verification and rendering functions. Every
sub-module remains directly importable for users who need more fine-grained
control.

= Quick start

Build an IR module containing a function that returns 0:

@
import LLVM.IR

example :: Text
example = compileModule "example" $
define void "hello" [] LExternal [] $ do
  beginBlock "entry"
  retVoid
@

Run the builder with explicit error handling using 'compileModuleWith':

@
case compileModuleWith "example" builder of
Left err -> putStrLn (displayError err)
Right ir  -> Data.Text.IO.putStr ir
@

= Operand constants

'LLVM.IRType.Constructors' (re-exported here) provides type-level
constructors such as @i32 :: IRType@ and @ptr :: IRType@.
'LLVM.IROperand.Constructors' provides identically-named value-level
constructors — @i32 :: Integral a => a -> IROperand@,
@float :: Float -> IROperand@ — that would create a name clash.

To avoid this, 'LLVM.IROperand.Constructors' is __not__ re-exported.
Import it qualified whenever you need constant operand helpers:

@
import LLVM.IR
import qualified LLVM.IROperand.Constructors as C

example :: IRBuilder ()
example =
define i32 "add" [(i32, "a"), (i32, "b")] LExternal [] $ do
  beginBlock "entry"
  r <- add i32 (C.i32 1) (C.i32 2)
  ret r
@

Alternatively, use the 'OConstant' and 'CInt' constructors re-exported
from 'LLVM.IROperand' directly:

@
ret (OConstant (CInt 32 0))
@

= Key concepts

['IRBuilder', 'IRBuilderT']
The builder monad. Run it with 'compileModule' (produces LLVM assembly as
'Text') or 'buildModule' (produces an 'IRModule'). Use 'compileModuleWith'
and 'buildModuleWith' for 'Either'-based error handling.

['define']
Declare a function. Takes return type, name, @[(type, paramName)]@ pairs,
'IRLinkage', @['IRAttribute']@, and a monadic body computation.

['beginBlock', 'block']
Open a new basic block. Use 'block' inside @mdo@ notation to capture the
generated label for use as a branch target — see the extended example.

[Instruction constructors]
Re-exported from 'LLVM.IRInstruction.Constructors'. Each function (e.g.
'add', 'load', 'icmp', 'phi', 'call') emits one instruction and returns
the result as an 'IROperand'.

['ret', 'br', 'condbr', 'switch', 'unreachable']
Terminators from 'LLVM.IRTerminator.Constructors'. Every basic block must
end with exactly one terminator.

[Type constructors]
From 'LLVM.IRType.Constructors': 'i1', 'i8', 'i16', 'i32', 'i64',
'i128', 'ptr', 'void', 'float', 'double', 'struct', 'array', 'vector',
'fun', 'named'.

['emitAnnotation', 'comment', 'commentBlock', '<##>']
Emit block-level comment annotations or attach an inline comment to the
preceding instruction with the @\<##\>@ operator.

= Extended example

An iterative factorial using @mdo@ for forward block references. The
pattern @loopLabel \<- block "loop"@ captures the generated unique label
so it can be referenced in branch targets and phi nodes that appear
lexically earlier in the @mdo@ block.

@
{\-# LANGUAGE OverloadedStrings #-\}
{\-# LANGUAGE RecursiveDo       #-\}

import LLVM.IR
import qualified LLVM.IROperand.Constructors as C

factorial :: Text
factorial = compileModule "mymod" $
define i64 "fact" [(i64, "n")] LExternal [] $ mdo
  beginBlock "entry"
  br loopLabel

  loopLabel <- block "loop"
  acc  <- phi i64 [(C.i64 1, "entry"),          (newAcc, bodyLabel)]
  n    <- phi i64 [(OLocal i64 "n", "entry"),   (newN,   bodyLabel)]
  cond <- icmp ICmpSGt i64 n (C.i64 0)
  condbr cond bodyLabel exitLabel

  bodyLabel <- block "body"
  newAcc <- mul i64 acc n
  newN   <- sub i64 n (C.i64 1)
  br loopLabel

  exitLabel <- block "exit"
  ret acc
@

= Module reference

['LLVM.IRBuilder'] Builder monad and compilation functions: 'compileModule',
'buildModule', 'compileModuleWith', 'buildModuleWith', 'define',
'beginBlock', 'block', 'emitInstruction', 'emitAnnotation',
'emitTerminator', 'emitGlobal', 'emitTypeDecl', 'declare',
'declareVarArg', and the @\<##\>@ inline-comment operator.

['LLVM.IRBuilder.Error'] 'IRBuilderError' type and 'displayError' for
programs that use explicit error handling via 'compileModuleWith' or
'buildModuleWith'.

['LLVM.IRInstruction'] IR instruction data types and condition codes:
'IRICmpCond', 'IRFCmpCond', 'IRTailMarker', 'IRInstrOp', 'IRInstruction'.

['LLVM.IRInstruction.Constructors'] Instruction smart constructors: 'add',
'sub', 'mul', 'sdiv', 'udiv', 'load', 'store', 'gep', 'call', 'callVoid',
'icmp', 'fcmp', 'phi', 'select', 'bitcast', 'sext', 'zext', 'trunc',
'alloca', 'freeze', 'atomicRMW', 'cmpXchg', 'fence', and more.

['LLVM.IRTerminator.Constructors'] Block terminators: 'ret', 'retVoid',
'br', 'condbr', 'switch', 'unreachable'.

['LLVM.IRType'] The 'IRType' algebraic data type and the 'IRName'
type alias (@type IRName = Text@).

['LLVM.IRType.Constructors'] Type smart constructors: 'i1', 'i8', 'i16',
'i32', 'i64', 'i128', 'ptr', 'void', 'float', 'double', 'struct',
'array', 'vector', 'fun', 'named'.

['LLVM.IRModule'] Module-level IR data types ('IRModule', 'IRFunction',
'IRBlock', 'IRGlobal', 'IRLinkage', 'IRAttribute', 'IRBlockItem') and
validation ('verifyModule', 'typeCheckModule').

['LLVM.IROperand'] Operand and constant data types: 'IROperand', 'IRConstant',
'IRTerminator'. Helper functions: 'operandType', 'constantType',
'opComponents'.

['LLVM.IRAnnotation'] The 'IRAnnotation' type ('Comment', 'CommentBlock').

['LLVM.IRAnnotation.Constructors'] Annotation helpers: 'comment',
'commentBlock', 'withComment'. The @\<##\>@ operator comes from
'LLVM.IRBuilder'.

['LLVM.IROperand.Constructors'] Integer and floating-point operand
constants (@i32@, @i64@, @float@, @local@, @global@, etc.).
__Not re-exported here__ — import this module qualified to avoid
collisions with the type constructors above.
-}
module LLVM.IR (
  -- * Builder monad and compilation
  module LLVM.IRBuilder,
  module LLVM.IRBuilder.Error,

  -- * Instruction types and condition codes
  IRICmpCond (..),
  IRFCmpCond (..),
  IRTailMarker (..),
  IRInstrOp (..),
  IRInstruction (..),

  -- * Instruction constructors
  module LLVM.IRInstruction.Constructors,

  -- * Terminator constructors
  module LLVM.IRTerminator.Constructors,

  -- * Type system
  module LLVM.IRType,
  module LLVM.IRType.Constructors,

  -- * IR data types
  module LLVM.IRModule,
  module LLVM.IROperand,

  -- * Annotations
  module LLVM.IRAnnotation,
  comment,
  commentBlock,
  withComment,
)
where

import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRAnnotation.Constructors (comment, commentBlock, withComment)
import LLVM.IRBuilder
import LLVM.IRBuilder.Error (IRBuilderError (..), displayError)
import LLVM.IRInstruction (IRFCmpCond (..), IRICmpCond (..), IRInstrOp (..), IRInstruction (..), IRTailMarker (..))
import LLVM.IRInstruction.Constructors
import LLVM.IRModule (IRAttribute (..), IRBlock (..), IRBlockItem (..), IRFunction (..), IRGlobal (..), IRLinkage (..), IRModule (..), IRTypeDecl (..), typeCheckModule, verifyModule)
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..), constantType, opComponents, operandType)
import LLVM.IRTerminator.Constructors
import LLVM.IRType (IRName, IRType (..))
import LLVM.IRType.Constructors
import Prelude hiding (and, or)
