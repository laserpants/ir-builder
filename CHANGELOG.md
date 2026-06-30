# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0.0] - 2026-06-30

### Added

- Initial release of ir-builder
- Monadic DSL for constructing LLVM IR programmatically
- Complete instruction set support including:
  - Arithmetic operations (add, sub, mul, div, rem)
  - Bitwise operations (and, or, xor, shifts)
  - Floating-point operations (fadd, fsub, fmul, fdiv, fneg)
  - Comparison operations (icmp, fcmp)
  - Memory operations (alloca, load, store, gep)
  - Type conversion operations (bitcast, sext, zext, trunc, etc.)
  - Control flow (phi, select, branch, conditional branch, switch)
  - Function calls (call, callVoid with tail call support)
  - Aggregate operations (extractValue, insertValue)
  - Vector operations (extractElement, insertElement, shuffleVector)
  - Atomic operations (atomicRMW, cmpXchg, fence)
- Terminators (ret, retVoid, br, condbr, switch, unreachable)
- Annotation support for inline comments via the `(<##>)` operator
- Pure renderer for serializing IR modules to LLVM assembly text
- Comprehensive error handling with typed IRBuilderError
- RecursiveDo support for forward references in phi nodes and loops
- Function and global declaration support
- Examples demonstrating common patterns

[Unreleased]: https://codeberg.org/laserpants/ir-builder/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://codeberg.org/laserpants/ir-builder/releases/tag/v0.1.0.0
