{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Examples.Factorial (factorialModule) where

import LLVM.IRBuilder
import LLVM.IRInstruction (IRICmpCond (..), IRTailMarker (..))
import LLVM.IRInstruction.Constructors (call, icmp, mul, phi, sub, trunc)
import LLVM.IRModule (IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..))
import LLVM.IRTerminator.Constructors (br, condbr, ret)
import LLVM.IRType.Constructors (i32, i64)

-- | Build LLVM IR for a 64-bit iterative factorial function plus a @main
-- that calls @fact(5) and returns the result.
--
-- Emits:
--
-- > define i64 @fact(i64 %n) {
-- > entry:
-- >   br label %loop
-- > loop:
-- >   %1 = phi i64 [ 1, %entry ], [ %4, %body ]
-- >   %2 = phi i64 [ %n, %entry ], [ %5, %body ]
-- >   %3 = icmp sgt i64 %2, 0
-- >   br i1 %3, label %body, label %exit
-- > body:
-- >   %4 = mul i64 %1, %2
-- >   %5 = sub i64 %2, 1
-- >   br label %loop
-- > exit:
-- >   ret i64 %1
-- > }
-- >
-- > define i32 @main() {
-- > entry:
-- >   %6 = call i64 @fact(i64 5)
-- >   %7 = trunc i64 %6 to i32
-- >   ret i32 %7
-- > }
factorialModule :: IRBuilder ()
factorialModule = do
  define i64 "fact" [(i64, "n")] LExternal [] $ mdo
    beginBlock "entry"
    br "loop"

    beginBlock "loop"
    accPhi <- phi i64 [(OConstant (CInt 64 1), "entry"), (newAcc, "body")]
    nPhi <- phi i64 [(OLocal i64 "n", "entry"), (newN, "body")]
    cond <- icmp ICmpSGt i64 nPhi (OConstant (CInt 64 0))
    condbr cond "body" "exit"

    beginBlock "body"
    newAcc <- mul i64 accPhi nPhi
    newN <- sub i64 nPhi (OConstant (CInt 64 1))
    br "loop"

    beginBlock "exit"
    ret accPhi

  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    result <- call NoTail i64 (OGlobal i64 "fact") [OConstant (CInt 64 5)]
    result32 <- trunc result i32
    ret result32
