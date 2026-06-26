# Coding style guide

This document describes the conventions and idioms used in this project.
Follow these when contributing or extending the library.

## Formatting

Formatting is enforced by **fourmolu**. Run it before committing.

- **Indentation**: 2 spaces (set in `fourmolu.yaml`).
- **Line length**: No hard limit, but keep lines readable. Aim for ~100 characters.
- **Trailing commas**: Not used. Lists, records, and import groups close on their own line.
- **Record fields**: Each field on its own line, aligned with the opening brace:

  ```haskell
  data IRModule = IRModule
    { moduleName :: IRName
    , moduleTypeDecls :: [IRTypeDecl]
    , moduleGlobals :: [IRGlobal]
    , moduleFunctions :: [IRFunction]
    }
    deriving (Show, Eq, Ord)
  ```

- **Multi-line expressions**: Break after operators and align continuation lines:

  ```haskell
  "getelementptr "
    <> renderType typ
    <> ", ptr "
    <> renderOperand base
    <> foldMap (", " <>) (map renderTypedOperand idxs)
  ```

## Language extensions

Extensions are declared **per file**, never in `package.yaml`. Only enable what the file actually uses.

| Extension | Files where used |
|---|---|
| `StrictData` | All data-type modules (`IRInstruction`, `IRModule`, `IROperand`, `IRType`) |
| `LambdaCase` | Renderer, type-checker, anything with a `\case` expression |
| `OverloadedStrings` | Renderer and tests (heavy `Text` literal use) |
| `NamedFieldPuns` | Builder and renderer (destructuring records by field name) |
| `RecordWildCards` | Builder and renderer (pattern-matching large records) |
| `GeneralizedNewtypeDeriving` | `IRBuilderT` newtype wrapper |
| `UndecidableInstances` | MTL lift-through instances |

Declare extensions at the top of the file, one per line, sorted alphabetically:

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
```

## GHC warnings

All warnings listed in `package.yaml` are treated as meaningful. Do not suppress them with
`{-# OPTIONS_GHC -Wno-... #-}` unless unavoidable, and leave a comment explaining why.

The enabled warning set includes:

```
-Wall -Wcompat -Widentities -Wincomplete-record-updates
-Wincomplete-uni-patterns -Wmissing-export-lists
-Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
```

`-Wmissing-export-lists` means **every module must have an explicit export list**.

## Module structure

### Export list

Always explicit, grouped by concept with section comments:

```haskell
module LLVM.IRBuilder (
  -- * Core types
  IRBuilderT (..),
  IRBuilder,
  MonadIRBuilder (..),
  runIRBuilder,

  -- * Function definition
  define,
  beginFunction,
  endFunction,

  -- * Block management
  beginBlock,
  block,
  finalizeCurrentBlock,

  -- * Error handling
  getCurrentBlockM,
  getCurrentFunctionM,
  liftEither,
) where
```

Export re-exported types in the section where they are conceptually used, not at the bottom.

### Import order

1. Standard library and `base` packages.
2. External packages (`Data.*`, `Control.*`, `Text.*`, `GHC.*`).
3. Internal library modules (`LLVM.*`), sorted alphabetically.

Each tier is separated by a blank line. Qualified imports share the tier with their unqualified siblings:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text

import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder.Class (MonadIRBuilder (..))
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
```

Prefer **selective imports** over `(..)` for non-ADT values. Use `(..)` when importing all constructors of a data type or all methods of a typeclass.

## Naming

| Category | Convention | Examples |
|---|---|---|
| Types and type constructors | `PascalCase`, `IR` prefix | `IRModule`, `IRFunction`, `IRType`, `IROperand` |
| Instruction opcodes | `PascalCase`, `I` prefix | `IAdd`, `ICall`, `IGep`, `IAtomicRMW` |
| Enum constructors | `PascalCase`, no prefix | `NoTail`, `Tail`, `MustTail`; `LExternal`, `LInternal` |
| Atomic ordering/op constructors | `PascalCase` | `SeqCst`, `AcqRel`; `ARMWAdd`, `ARMWXchg` |
| Record fields | `camelCase`, module-prefixed | `moduleName`, `instrOp`, `blockLabel`, `typeDeclName` |
| Functions and smart constructors | `camelCase` | `add`, `emitInstruction`, `renderModule`, `operandType` |
| Type aliases | `PascalCase`, `IR` prefix | `IRName`, `IRBuilder` |

Prefix record fields with an abbreviated module/type name to avoid ambiguity
(`instrResult`, not `result`; `blockLabel`, not `label`).

## Data types

### ADTs

One constructor per line. Constructors are sorted alphabetically inside a data type where
no semantic ordering applies (e.g. `IRInstrOp`):

```haskell
data IRInstrOp
  = IAShr IRType IROperand IROperand
  | IAdd IRType IROperand IROperand
  | IAlloca IRType IROperand
  | IAnd IRType IROperand IROperand
  | IBitcast IROperand IRType
  | ICall IRTailMarker IRType IROperand [IROperand]
  ...
  deriving (Show, Eq, Ord)
```

### Records

Use record syntax for any data type with more than two fields.
Do not use positional syntax for records—always pattern match by field name:

```haskell
renderFunction IRFunction{functionName, functionRetType, functionBlocks} = ...
```

### Deriving

The standard deriving set for IR data types is `(Show, Eq, Ord)`. Add `Functor` or other
classes only when actively needed.

### StrictData

All data-type modules use `{-# LANGUAGE StrictData #-}`. Fields are strict by default,
which avoids accidental space leaks in the IR graph.

## Typeclasses and MTL

The builder DSL is structured around a single typeclass `MonadIRBuilder`. Always program
to this interface; never to the concrete `IRBuilderT`:

```haskell
-- Good
add :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand

-- Avoid
add :: IRType -> IROperand -> IROperand -> IRBuilder IROperand
```

Lift-through instances for standard transformers (`StateT`, `ExceptT`, `ReaderT`) are
required when defining a new `MonadIRBuilder` operation so the interface composes freely.

The monad stack is `StateT IRBuilderEnv (ExceptT IRBuilderError m)`. Error propagation
uses `throwIRBuilderError`; recovery is not expected inside the builder.

## Smart constructors

All instruction-emitting functions live in `LLVM.IRInstruction.Constructors` and follow
this pattern:

```haskell
-- | Add two integers.
add :: (MonadIRBuilder m) => IRType -> IROperand -> IROperand -> m IROperand
add t a b = emitWithResult t (IAdd t a b)
```

Use `emitWithResult` for instructions that produce a result register, and `emitVoid` for
instructions with no result (e.g. `store`, `fence`):

```haskell
emitWithResult :: (MonadIRBuilder m) => IRType -> IRInstrOp -> m IROperand
emitVoid       :: (MonadIRBuilder m) => IRInstrOp -> m ()
```

When the result type can be derived from an operand's type, do so rather than requiring
the caller to supply it:

```haskell
freeze :: (MonadIRBuilder m) => IROperand -> m IROperand
freeze op = emitWithResult (operandType op) (IFreeze op)
```

## Documentation

Use **Haddock block comments** (`{- | ... -}`) for all exported declarations. Line
comments (`-- |`) are acceptable for short single-line docs on record fields.

Module-level documentation goes immediately after the `module` declaration:

```haskell
{- |
This module defines the fundamental data structures that represent compiled
LLVM IR. It is the output produced by the 'LLVM.IRBuilder' DSL and serves
as the canonical representation of an IR module.

= Example

@
m <- buildModule "my_module" $ do
    define i32 "main" [] $ do
        block "entry"
        ret (OConstant (CInt 32 0))
@
-}
module LLVM.IRModule (...) where
```

Document record fields in the data-type Haddock using a `__Fields:__` section:

```haskell
{- | An LLVM IR module — the top-level compilation unit.

__Fields:__

* 'moduleName': The name of this module.
* 'moduleTypeDecls': Named type declarations (@%Node = type { ... }@).
* 'moduleFunctions': Function definitions.
-}
data IRModule = IRModule { ... }
```

## Common idioms

### LambdaCase

Use `\case` instead of a named parameter when a function immediately pattern-matches its
sole argument. This is the dominant style in the renderer:

```haskell
renderAttribute :: IRAttribute -> Text
renderAttribute =
  \case
    NoReturn -> "noreturn"
    NoUnwind -> "nounwind"
    ReadOnly -> "readonly"
```

### OverloadedStrings

Always use `OverloadedStrings` in any module that constructs `Text` values with string
literals. Never call `Text.pack` on a literal.

### Text construction

Prefer `<>` and `foldMap` for building `Text`. Use `Text.intercalate` when joining a list
with a separator:

```haskell
Text.intercalate ", " (map renderTypedOperand args)
foldMap (", " <>) (map (Text.pack . show) idxs)
```

### INLINE pragmas on type aliases

Nullary constructor helpers in `LLVM.IRType.Constructors` carry `{-# INLINE #-}` to
eliminate any overhead at call sites:

```haskell
{-# INLINE i32 #-}
i32 :: IRType
i32 = TInt 32
```

Apply the same pattern to any zero-argument function that is purely an alias.

## Testing

Tests use **hspec** with **hspec-discover**. Each source module `LLVM.Foo` has a
corresponding spec `Unit.LLVM.FooSpec` under `test/Unit/LLVM/`.

### File structure

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Instruction.ConstructorsSpec (spec) where

import ...
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

-- Shared helpers and fixtures
a32, b32 :: IROperand
a32 = OLocal (TInt 32) "a"
b32 = OLocal (TInt 32) "b"

spec :: Spec
spec = describe "LLVM.IRInstruction.Constructors" $ do
  describe "Arithmetic" $ do
    it "add emits IAdd" $ do
      ...
```

### Describe hierarchy

Use two levels: the top-level `describe` names the module under test; nested `describe`
blocks group related tests by feature or concept (`"Arithmetic"`, `"Memory"`, `"Atomics"`).

### Assertions

Prefer `shouldBe` for exact equality. Use `shouldContain` when testing rendered output
(a substring match is sufficient and more robust). Avoid `shouldSatisfy` unless no
equality check is possible.

For void-returning builder actions (e.g. `store`, `fence`), access the environment
directly via `runBuilder` rather than through the `runInstrBuilder` helper:

```haskell
it "fence emits IFence (no result)" $ do
  let initialEnv = emptyIRBuilderEnv { builderEnvCurrentBlock = Just ... }
      (_, finalEnv) = case runBuilder (fence AcqRel) initialEnv of
        Right (_, e) -> ((), e)
        Left err     -> error (show err)
      items = maybe [] blockBuilderItems (builderEnvCurrentBlock finalEnv)
  lastInstrOp items `shouldBe` Just (IFence AcqRel)
```

## Architecture layers

The codebase is divided into four layers; dependencies only flow downward:

```
IRType  ←  IROperand  ←  IRInstruction  ←  IRModule

                                       ↓
                                       
                                   IRBuilder  →  IRRenderer
```

| Layer | Responsibility |
|---|---|
| `IRType` | Primitive type lattice (`TInt`, `TFloat`, `TPtr`, `TVector`, …) |
| `IROperand` | Operands, constants, terminators; `operandType`/`constantType` |
| `IRInstruction` | Opcode ADT and metadata-parameterised instruction wrapper |
| `IRModule` | Module, function, block, global data structures; `verifyModule`; `typeCheckModule` |
| `IRBuilder` | Monadic DSL — emitting instructions, naming registers, managing blocks |
| `IRRenderer` | Pure `Text` serialisation of `IRModule` to LLVM assembly syntax |

Sub-modules (`IRBuilder/Class.hs`, `IRBuilder/Environment.hs`, etc.) expose internal
components without polluting the primary module's namespace. Re-export what the public
API needs from the top-level module.
