module Examples.Factorial (factorialModule) where

import LLVM.IR (IRBuilder)
import LLVM.IR.Examples (factorial)

factorialModule :: IRBuilder ()
factorialModule = factorial
