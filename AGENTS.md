# Agent Guide — ir-builder

> Quick reference for AI coding agents generating Haskell code that builds
> LLVM IR using this library. Load this file as context before generating code.

## Import

```haskell
import LLVM.IR                                     -- complete public API
import qualified LLVM.IROperand.Constructors as C  -- constant helpers (optional)
```

Without `C`: use `OConstant (CInt 32 0)` for integer constants and
`OConstant (CFloat 1.0)` for floats. With `C`: use `C.i32 0`, `C.i64 1`,
`C.float 1.0`, etc.

## Invariants

These rules must hold for every generated module. Violations produce typed
`IRBuilderError` values rather than silent wrong output.

1. **Every block needs a terminator.** Each basic block must end with
   exactly one of `ret`, `retVoid`, `br`, `condbr`, `switch`, or
   `unreachable`. A block without a terminator raises
   `BlockMissingTerminator "blockName"` when finalized. A block with two
   terminators raises `BlockAlreadyTerminated "blockName"`.

2. **The first block is `"entry"`.** Always call `beginBlock "entry"` as the
   first statement inside every `define` body. This is convention, not
   enforcement, but LLVM tooling expects it.

3. **Parameters are `OLocal`.** A parameter declared as `[(i32, "x")]` in
   `define` is accessed as `OLocal i32 "x"`. The name string must match
   exactly — including case.

4. **Results are SSA bindings.** Capture instruction results with `<-`
   and pass the `IROperand` binding directly. Never attempt to reconstruct
   a result operand from a name string.

5. **Use `mdo` for forward references.** Enable
   `{-# LANGUAGE RecursiveDo #-}` and use `mdo` when a phi source or branch
   target is a Haskell binding defined **later** in the same block sequence.
   Use `do` when there are no such forward references (branches, linear code).

6. **Declare before calling.** Invoke `declare` or `declareVarArg` before
   any `call` or `callVoid` on external functions. Duplicate declarations are
   silently ignored, so calling `declare` unconditionally is safe.

## Operands

| How to get an `IROperand` | Code |
|---|---|
| Function parameter `x :: i32` | `OLocal i32 "x"` |
| Global / declared function | `OGlobal i64 "fact"`, `OGlobal TPtr ".str"` |
| Integer constant | `OConstant (CInt 32 0)` or `C.i32 0` |
| Float constant | `OConstant (CFloat 1.0)` or `C.float 1.0` |
| Instruction result | Capture: `r <- add i32 a b` — pass `r` directly |

## `do` vs `mdo`

Use **`do`** when there are no Haskell forward references — branches,
linear code, and `phi` nodes whose back-edge sources are all string literals:

```haskell
define i32 "abs" [(i32, "x")] LExternal [] $ do
  beginBlock "entry"
  cond <- icmp ICmpSGe i32 (OLocal i32 "x") (OConstant (CInt 32 0))
  condbr cond "nonneg" "neg"   -- string literals for targets — no forward ref
  beginBlock "nonneg"
  ret (OLocal i32 "x")
  beginBlock "neg"
  r <- sub i32 (OConstant (CInt 32 0)) (OLocal i32 "x")
  ret r
```

Use **`mdo`** when a phi source or branch target is a Haskell binding
defined later in the same block sequence (loop back-edges, captured labels):

```haskell
{-# LANGUAGE RecursiveDo #-}
define i64 "f" [(i64, "n")] LExternal [] $ mdo
  beginBlock "loop"
  acc  <- phi i64 [(OConstant (CInt 64 1), "entry"), (newAcc, "body")]
  --                                                   ^^^^^^ defined below — forward ref
  beginBlock "body"
  newAcc <- mul i64 acc (OLocal i64 "n")   -- newAcc referenced above
  br "loop"
```

**Rule of thumb:** if any Haskell binding from the `do` block appears as a phi
source or branch target before the line where it is bound, use `mdo`.

## Pattern cookbook

### 1. Linear (sequence)

```haskell
define i32 "square" [(i32, "x")] LExternal [] $ do
  beginBlock "entry"
  r <- mul i32 (OLocal i32 "x") (OLocal i32 "x")
  ret r
```

```llvm
define i32 @square(i32 %x) {
entry:
  %1 = mul i32 %x, %x
  ret i32 %1
}
```

### 2. Conditional branch — two exit points

```haskell
define i32 "abs" [(i32, "x")] LExternal [] $ do
  beginBlock "entry"
  cond <- icmp ICmpSGe i32 (OLocal i32 "x") (OConstant (CInt 32 0))
  condbr cond "nonneg" "neg"

  beginBlock "nonneg"
  ret (OLocal i32 "x")

  beginBlock "neg"
  r <- sub i32 (OConstant (CInt 32 0)) (OLocal i32 "x")
  ret r
```

```llvm
define i32 @abs(i32 %x) {
entry:
  %1 = icmp sge i32 %x, 0
  br i1 %1, label %nonneg, label %neg
nonneg:
  ret i32 %x
neg:
  %2 = sub i32 0, %x
  ret i32 %2
}
```

### 3. Conditional with phi merge — single exit, selected value

```haskell
define i32 "max" [(i32, "a"), (i32, "b")] LExternal [] $ do
  beginBlock "entry"
  cond <- icmp ICmpSGt i32 (OLocal i32 "a") (OLocal i32 "b")
  condbr cond "then_" "else_"

  beginBlock "then_"
  br "merge"

  beginBlock "else_"
  br "merge"

  beginBlock "merge"
  r <- phi i32 [(OLocal i32 "a", "then_"), (OLocal i32 "b", "else_")]
  ret r
```

```llvm
define i32 @max(i32 %a, i32 %b) {
entry:
  %1 = icmp sgt i32 %a, %b
  br i1 %1, label %then_, label %else_
then_:
  br label %merge
else_:
  br label %merge
merge:
  %2 = phi i32 [ %a, %then_ ], [ %b, %else_ ]
  ret i32 %2
}
```

### 4. Loop with back-edge phi (`mdo`)

Phi sources `iNext` and `accNext` are defined in `"body"` but referenced
in `"loop"` — requires `mdo`.

```haskell
{-# LANGUAGE RecursiveDo #-}

define i64 "sum_to_n" [(i64, "n")] LExternal [] $ mdo
  beginBlock "entry"
  br "loop"

  beginBlock "loop"
  i    <- phi i64 [(OConstant (CInt 64 0), "entry"), (iNext,   "body")]
  acc  <- phi i64 [(OConstant (CInt 64 0), "entry"), (accNext, "body")]
  cond <- icmp ICmpSLt i64 i (OLocal i64 "n")
  condbr cond "body" "exit"

  beginBlock "body"
  iNext   <- add i64 i   (OConstant (CInt 64 1))
  accNext <- add i64 acc i
  br "loop"

  beginBlock "exit"
  ret acc
```

```llvm
define i64 @sum_to_n(i64 %n) {
entry:
  br label %loop
loop:
  %1 = phi i64 [ 0, %entry ], [ %4, %body ]
  %2 = phi i64 [ 0, %entry ], [ %5, %body ]
  %3 = icmp slt i64 %1, %n
  br i1 %3, label %body, label %exit
body:
  %4 = add i64 %1, 1
  %5 = add i64 %2, %1
  br label %loop
exit:
  ret i64 %2
}
```

`i` → `%1`, `acc` → `%2`, `cond` → `%3`, `iNext` → `%4`, `accNext` → `%5`.

### 5. External function call

```haskell
-- Declare before use; duplicates are safe.
declare "strlen" i64 [ptr]

define i64 "my_strlen" [(ptr, "s")] LExternal [] $ do
  beginBlock "entry"
  r <- call NoTail i64 (OGlobal i64 "strlen") [OLocal ptr "s"]
  ret r
```

```llvm
declare i64 @strlen(ptr)

define i64 @my_strlen(ptr %s) {
entry:
  %1 = call i64 @strlen(ptr %s)
  ret i64 %1
}
```

For variadic functions: `declareVarArg "printf" i32 [ptr]` and
`callVoid NoTail i32 (OGlobal i32 "printf") [OGlobal ptr ".fmt", result]`.

## Instruction signatures

All constructors require `(MonadIRBuilder m)`.

```haskell
-- Arithmetic   (type → lhs → rhs → result)
add, sub, mul, sdiv, udiv, srem, urem :: IRType -> IROperand -> IROperand -> m IROperand

-- Bitwise
and, or, xor, shl, lshr, ashr :: IRType -> IROperand -> IROperand -> m IROperand

-- Floating-point
fadd, fsub, fmul, fdiv :: IRType -> IROperand -> IROperand -> m IROperand
fneg                   :: IRType -> IROperand -> m IROperand

-- Comparison (result is always i1)
icmp :: IRICmpCond -> IRType -> IROperand -> IROperand -> m IROperand
fcmp :: IRFCmpCond -> IRType -> IROperand -> IROperand -> m IROperand

-- Memory
alloca :: IRType -> IROperand {- count -} -> m IROperand
load   :: IRType -> IROperand {- ptr   -} -> m IROperand
store  :: IROperand {- value -} -> IROperand {- ptr -} -> m ()
gep    :: IRType -> IROperand {- base  -} -> [IROperand] {- indices -} -> m IROperand

-- Type casts   (source → target type → result)
bitcast, inttoptr, ptrtoint :: IROperand -> IRType -> m IROperand
sext, zext, trunc           :: IROperand -> IRType -> m IROperand

-- Calls
call     :: IRTailMarker -> IRType {- ret -} -> IROperand {- fn -} -> [IROperand] -> m IROperand
callVoid :: IRTailMarker -> IRType {- ret -} -> IROperand {- fn -} -> [IROperand] -> m ()

-- Misc
phi    :: IRType -> [(IROperand, IRName {- predecessor label -})] -> m IROperand
select :: IRType -> IROperand {- i1 -} -> IROperand -> IROperand -> m IROperand
freeze :: IROperand -> m IROperand

-- Aggregates
extractValue :: IRType -> IROperand -> [Int] -> m IROperand
insertValue  :: IROperand -> IROperand -> [Int] -> m IROperand

-- Vectors
extractElement :: IRType -> IROperand -> IROperand -> m IROperand
insertElement  :: IROperand -> IROperand -> IROperand -> m IROperand
shuffleVector  :: IRType -> IROperand -> IROperand -> [Int] -> m IROperand

-- Atomics
atomicRMW :: IRAtomicOrdering -> IRAtomicOp -> IROperand -> IROperand -> m IROperand
cmpXchg   :: IRAtomicOrdering -> IRAtomicOrdering -> IROperand -> IROperand -> IROperand -> m IROperand
fence     :: IRAtomicOrdering -> m ()

-- Terminators
ret         :: IROperand -> m ()
retVoid     :: m ()
br          :: IRName -> m ()
condbr      :: IROperand {- i1 -} -> IRName -> IRName -> m ()
switch      :: IROperand -> IRName {- default -} -> [(IRConstant, IRName)] -> m ()
unreachable :: m ()
```

`IRTailMarker`:
`NoTail` | `Tail` | `MustTail`

`IRICmpCond`:
`ICmpEq` | `ICmpNe` | `ICmpUGt` | `ICmpUGe` | `ICmpULt` | `ICmpULe` | `ICmpSGt` | `ICmpSGe` | `ICmpSLt` | `ICmpSLe`

`IRAtomicOrdering`:
`Unordered` | `Monotonic` | `Acquire` | `Release` | `AcqRel` | `SeqCst`

`IRAtomicOp`:
`ARMWAdd` | `ARMWSub` | `ARMWAnd` | `ARMWOr` | `ARMWXor` | `ARMWXchg` | `ARMWMax` | `ARMWMin` | `ARMWUMax` | `ARMWUMin` | `ARMWFAdd` | `ARMWFSub` | `ARMWFMax` | `ARMWFMin` | `ARMWNand`

## Error reference

| Error | Cause | Fix |
|---|---|---|
| `BlockMissingTerminator "x"` | Block ended without `ret`/`br`/etc. | Add terminator as last instruction in block |
| `BlockAlreadyTerminated "x"` | Two terminators in one block | Remove the duplicate |
| `NoInstructionForComment` | `(<##>)` after annotation or terminator | Only use `(<##>)` after result-producing instructions |
| `CommentOnAnnotation` | `(<##>)` immediately after `emitAnnotation` | Same — `(<##>)` only attaches to instructions |
| `NoCurrentBlock` | `(<##>)` before any `beginBlock` | Call `beginBlock` first |
| `CurrentFunctionActive "f"` | `define` nested inside `define` | Do not nest function definitions |
| `NoCurrentFunction` | `endFunction` without matching `beginFunction` | Use high-level `define` instead of manual begin/end |
