# coal-llvm-subsystem

A Haskell library for constructing LLVM IR programmatically. It provides a
monadic DSL for building modules, functions, blocks, and instructions, along
with a pure renderer that serializes the result to LLVM assembly.

## Overview

The library is structured as a pipeline:

```
IRBuilder (monadic DSL)  →  IRModule (data types)  →  renderModule  →  Text
```

You describe an IR module using the `IRBuilder` monad. When you run
`compileModule`, the builder produces an `IRModule` value. `renderModule`
then converts that value to the textual LLVM IR format that can be passed
to `llc`, `clang`, or `lli`.

```haskell
import LLVM.IR

example :: Text
example = compileModule "example" $ do
  define i32 "add_one" [(i32, "x")] LExternal [] $ do
    beginBlock "entry"
    r <- add i32 (OLocal i32 "x") (OConstant (CInt 32 1))
    ret r
```

## Building

Requires [Stack](https://docs.haskellstack.org/).

```
stack build        # build the library
stack test         # run the test suite
```

## Quick example: Hello, World!

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Examples.HelloWorld (helloWorld) where

import LLVM.IR

helloWorld :: IRBuilder ()
helloWorld = do
  declare "puts" i32 [ptr]
  emitGlobal (IRString LPrivate ".str" "Hello, World!\0")

  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    r1 <- gep (TArray 14 i8) (OGlobal TPtr ".str")
              [OConstant (CInt 32 0), OConstant (CInt 32 0)]
    callVoid NoTail i32 (OGlobal i32 "puts") [r1]
    ret (OConstant (CInt 32 0))
```

Running `compileModule "hello_world" helloWorld` produces:

```llvm
@.str = private constant [14 x i8] c"Hello, World!\00"
declare i32 @puts(ptr)

define i32 @main() {
entry:
  %1 = getelementptr [14 x i8], ptr @.str, i32 0, i32 0
  call i32 @puts(ptr %1)
  ret i32 0
}
```

## Quick example: Iterative factorial with `mdo`

The `IRBuilder` monad supports `MonadFix`. You can use `mdo` to refer to
SSA values before they are emitted, which is necessary for `phi` nodes in
loops:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Examples.Factorial (factorialModule) where

import LLVM.IR

factorialModule :: IRBuilder ()
factorialModule = do
  declareVarArg "printf" i32 [ptr]
  emitGlobal (IRString LPrivate ".fmt" "fact(5) = %ld\n\0")

  define i64 "fact" [(i64, "n")] LExternal [] $ mdo
    beginBlock "entry"
    br "loop"

    beginBlock "loop"
    accPhi <- phi i64 [(OConstant (CInt 64 1), "entry"), (newAcc, "body")]
    nPhi   <- phi i64 [(OLocal i64 "n",        "entry"), (newN,   "body")]
    cond   <- icmp ICmpSGt i64 nPhi (OConstant (CInt 64 0))
    condbr cond "body" "exit"

    beginBlock "body"
    newAcc <- mul i64 accPhi nPhi
    newN   <- sub i64 nPhi (OConstant (CInt 64 1))
    br "loop"

    beginBlock "exit"
    ret accPhi

  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    result <- call NoTail i64 (OGlobal i64 "fact") [OConstant (CInt 64 5)]
    callVoid NoTail i32 (OGlobal i32 "printf") [OGlobal ptr ".fmt", result]
    ret (OConstant (CInt 32 0))
```

## Type system

LLVM types are represented by the `IRType` data type:

| Constructor | Description |
|---|---|
| `TInt n` | Integer with bit width `n` (e.g. `TInt 32`) |
| `TFloat` | 32-bit IEEE float |
| `TDouble` | 64-bit IEEE float |
| `TVoid` | Void type |
| `TPtr` | Opaque pointer |
| `TFun ret params` | Function type |
| `TStruct fields` | Struct (anonymous) |
| `TArray n elem` | Fixed-size array |
| `TVector n elem` | SIMD vector |
| `TNamed name` | Named type reference (e.g. `%Node`) |
| `TOpaque name` | Forward-declared opaque type |

`LLVM.IRType.Constructors` exports shorthand aliases: `i1`, `i8`, `i16`,
`i32`, `i64`, `i128`, `float`, `double`, `ptr`, `void`, and constructor
helpers `struct`, `array`, `vector`, `fun`, `named`.

## Linkage

`IRLinkage` controls symbol visibility and is the fourth argument to `define`:

| Value | Description |
|---|---|
| `LExternal` | Visible outside the module (default for most functions) |
| `LInternal` | Module-local; may be renamed to avoid clashes during linking |
| `LPrivate` | Module-local and excluded from the symbol table |

## Function attributes

`IRAttribute` values form the fifth argument to `define` as a list:

| Value | Description |
|---|---|
| `NoReturn` | Function never returns normally |
| `NoUnwind` | Function never throws (no stack unwinding) |
| `ReadOnly` | Function only reads memory, no writes |
| `ReadNone` | Function neither reads nor writes memory |
| `AlwaysInline` | Always inline at call sites |
| `NoInline` | Never inline |
| `TailCall` | Mark as eligible for tail-call optimisation |
| `MustTailCall` | Require tail-call elimination |
| `Cold` | Rarely executed path |
| `Hot` | Frequently executed path |
| `InlineHint` | Hint to inline |
| `NoAlias` | Return value does not alias any existing pointer |
| `GC Text` | Specify a garbage collector by name, e.g. `GC "shadow-stack"` |

## Instruction set

All instruction-emitting functions are in `LLVM.IRInstruction.Constructors`
and share the constraint `(MonadIRBuilder m)`. Terminator-emitting functions
are in `LLVM.IRTerminator.Constructors`.

### Arithmetic

```haskell
add, sub, mul, sdiv, udiv, srem, urem :: IRType -> IROperand -> IROperand -> m IROperand
```

### Bitwise

```haskell
and, or, xor, shl, lshr, ashr :: IRType -> IROperand -> IROperand -> m IROperand
```

### Floating-point

```haskell
fadd, fsub, fmul, fdiv :: IRType -> IROperand -> IROperand -> m IROperand
fneg                   :: IRType -> IROperand -> m IROperand
```

### Comparison

```haskell
icmp :: IRICmpCond -> IRType -> IROperand -> IROperand -> m IROperand
fcmp :: IRFCmpCond -> IRType -> IROperand -> IROperand -> m IROperand
```

### Memory

```haskell
alloca :: IRType -> IROperand -> m IROperand           -- alloca <type>, <count>
load   :: IRType -> IROperand -> m IROperand           -- load <type>, ptr <ptr>
store  :: IROperand -> IROperand -> m ()               -- store <val>, ptr <ptr>
gep    :: IRType -> IROperand -> [IROperand] -> m IROperand
```

### Type casts

```haskell
bitcast  :: IROperand -> IRType -> m IROperand
sext     :: IROperand -> IRType -> m IROperand
zext     :: IROperand -> IRType -> m IROperand
trunc    :: IROperand -> IRType -> m IROperand
inttoptr :: IROperand -> IRType -> m IROperand
ptrtoint :: IROperand -> IRType -> m IROperand
```

### Calls

```haskell
call     :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> m IROperand
callVoid :: IRTailMarker -> IRType -> IROperand -> [IROperand] -> m ()
```

`IRTailMarker` is one of `NoTail`, `Tail`, or `MustTail`.

### Miscellaneous

```haskell
phi    :: IRType -> [(IROperand, IRName)] -> m IROperand
select :: IRType -> IROperand -> IROperand -> IROperand -> m IROperand
freeze :: IROperand -> m IROperand
```

### Aggregates

```haskell
extractValue :: IRType -> IROperand -> [Int] -> m IROperand
insertValue  :: IROperand -> IROperand -> [Int] -> m IROperand
```

### Vectors

```haskell
extractElement :: IRType -> IROperand -> IROperand -> m IROperand
insertElement  :: IROperand -> IROperand -> IROperand -> m IROperand
shuffleVector  :: IRType -> IROperand -> IROperand -> [Int] -> m IROperand
```

Use `-1` in the mask to indicate an `undef` slot in `shuffleVector`.

### Atomics

```haskell
atomicRMW :: IRAtomicOrdering -> IRAtomicOp -> IROperand -> IROperand -> m IROperand
cmpXchg   :: IRAtomicOrdering -> IRAtomicOrdering -> IROperand -> IROperand -> IROperand -> m IROperand
fence     :: IRAtomicOrdering -> m ()
```

`IRAtomicOrdering`: `Unordered`, `Monotonic`, `Acquire`, `Release`, `AcqRel`, `SeqCst`.  
`IRAtomicOp`: `ARMWAdd`, `ARMWSub`, `ARMWAnd`, `ARMWOr`, `ARMWXor`, `ARMWXchg`,
`ARMWMax`, `ARMWMin`, `ARMWUMax`, `ARMWUMin`, `ARMWFAdd`, `ARMWFSub`, `ARMWFMax`,
`ARMWFMin`, `ARMWNand`.

`cmpXchg` returns an operand of type `{ <ty>, i1 }`.

### Terminators

```haskell
ret         :: IROperand -> m ()
retVoid     :: m ()
br          :: IRName -> m ()
condbr      :: IROperand -> IRName -> IRName -> m ()
switch      :: IROperand -> IRName -> [(IRConstant, IRName)] -> m ()
unreachable :: m ()
```

## Globals and declarations

```haskell
-- Declare an external function
declare :: IRName -> IRType -> [IRType] -> m ()

-- Declare a variadic external function
declareVarArg :: IRName -> IRType -> [IRType] -> m ()

-- Emit a named type declaration (%Name = type { ... })
emitTypeDecl :: IRName -> IRType -> m ()

-- Emit any IRGlobal directly
emitGlobal :: IRGlobal -> m ()
```

`IRGlobal` constructors:

| Constructor | Description |
|---|---|
| `IRString linkage name bytes` | String literal (stored as `[n x i8]`) |
| `IRConstant linkage name type val` | Immutable global |
| `IRVar linkage name type val` | Mutable global |
| `IRExtern name retType argTypes isVariadic` | External declaration |

## Annotations

Instructions can carry an `IRAnnotation` metadata value. The `(<##>)` operator
attaches a comment to an instruction result:

```haskell
r <- add i32 a b <##> "compute sum"
```

This renders as a trailing inline comment on the same line as the instruction:

```llvm
%1 = add i32 %a, %b  ; compute sum
```

## Verification

Two post-hoc checks are available on a compiled `IRModule`:

```haskell
verifyModule    :: IRModule -> Either String ()
typeCheckModule :: IRModule -> Either String ()
```

`verifyModule` checks structural properties: no duplicate block labels,
no duplicate SSA names within a function, and all branch targets are
defined blocks.

`typeCheckModule` checks that operand types are consistent with instruction
semantics: arithmetic operands match the declared type, `phi` incoming
values match the declared type, `load`/`store` pointer operands are `ptr`,
`alloca` counts are integers, sign-extension/truncation constraints hold,
and so on.

Neither function is called automatically by `compileModule` — you invoke
them when needed.

## Module reference

| Module | Contents |
|---|---|
| `LLVM.IR` | Single-import façade — re-exports the complete public API |
| `LLVM.IRBuilder` | Builder monad: `compileModule`, `define`, `beginBlock`, `emitInstruction`, `declare`, `declareVarArg`, `emitGlobal`, `emitTypeDecl`, `(<##>)` |
| `LLVM.IRInstruction.Constructors` | All instruction smart constructors |
| `LLVM.IRTerminator.Constructors` | `ret`, `retVoid`, `br`, `condbr`, `switch`, `unreachable` |
| `LLVM.IROperand.Constructors` | Constant and operand helpers (`int`, `float`, `nullPtr`, …) |
| `LLVM.IRType.Constructors` | Type aliases (`i32`, `ptr`, `float`, …) |
| `LLVM.IRAnnotation.Constructors` | Comment helpers: `comment`, `commentBlock`, `withComment` |
| `LLVM.IRRenderer` | `renderModule :: IRModule -> Text` |
| `LLVM.IRModule` | `IRModule`, `IRFunction`, `IRBlock`, `IRGlobal`, `IRLinkage`, `IRAttribute`, `verifyModule`, `typeCheckModule` |
| `LLVM.IRInstruction` | `IRInstrOp`, `IRInstruction`, `IRICmpCond`, `IRFCmpCond`, `IRTailMarker`, `IRAtomicOrdering`, `IRAtomicOp` |
| `LLVM.IROperand` | `IROperand`, `IRConstant`, `IRTerminator`, `operandType`, `constantType` |
| `LLVM.IRType` | `IRType` |
| `LLVM.IRBuilder.Class` | `MonadIRBuilder` typeclass (for transformer stacks) |
| `LLVM.IRBuilder.Error` | `IRBuilderError`, `displayError` |

## Dependencies

- `base`
- `text` — `Text` is used throughout for names and rendered output
- `bytestring` — string literal globals
- `containers` — `Set` in verification
- `mtl` — `StateT`, `ExceptT`, `MonadFix`
