{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

{- | Worked examples for the @ir-builder@ library.

Each function is a self-contained 'IRBuilder' action that constructs a
complete LLVM module. Pass any of them to 'compileModule' to produce LLVM
assembly:

@
import LLVM.IR
import LLVM.IR.Examples

main :: IO ()
main = Data.Text.IO.putStrLn (compileModule "hello" helloWorld)
@
-}
module LLVM.IR.Examples (
  helloWorld,
  addNumbers,
  factorial,
) where

import LLVM.IR

{- | A \"Hello, World!\" program using the C @puts@ function.

Declares @puts@, emits a private string constant, and defines @main@
which passes the string to @puts@ via 'gep':

@
helloWorld :: IRBuilder ()
helloWorld = do
  declare \"puts\" i32 [ptr]
  emitGlobal (IRString LPrivate \".str\" \"Hello, World!\\0\")
  define i32 \"main\" [] LExternal [] $ do
    beginBlock \"entry\"
    r1 <-
      gep
        (array 14 i8)
        (OGlobal ptr \".str\")
        [OConstant (CInt 32 0), OConstant (CInt 32 0)]
    callVoid NoTail i32 (OGlobal i32 \"puts\") [r1]
    ret (OConstant (CInt 32 0))
@

Produces:

> declare i32 @puts(ptr)
>
> @.str = private constant [14 x i8] c"Hello, World!\00"
>
> define i32 @main() {
> entry:
>   %1 = getelementptr [14 x i8], ptr @.str, i32 0, i32 0
>   call i32 @puts(ptr %1)
>   ret i32 0
> }
-}
helloWorld :: IRBuilder ()
helloWorld = do
  declare "puts" i32 [ptr]
  emitGlobal (IRString LPrivate ".str" "Hello, World!\0")
  define i32 "main" [] LExternal [] $ do
    beginBlock "entry"
    r1 <-
      gep
        (array 14 i8)
        (OGlobal ptr ".str")
        [OConstant (CInt 32 0), OConstant (CInt 32 0)]
    callVoid NoTail i32 (OGlobal i32 "puts") [r1]
    ret (OConstant (CInt 32 0))

{- | Simple addition function demonstrating 'emitAnnotation' and '(<##>)'.

Block comments (@;@) and inline comments are two distinct annotation
styles. Note that '(<##>)' attaches to instructions only — terminators
do not support it.

@
addNumbers :: IRBuilder ()
addNumbers =
  define i32 \"add_numbers\" [(i32, \"a\"), (i32, \"b\")] LExternal [] $ do
    beginBlock \"entry\"
    emitAnnotation
      (commentBlock
        [ \"Explanation: This block performs a simple addition\"
        , \"of two 32-bit integers\"
        ])
    result <- add i32 (OLocal i32 \"a\") (OLocal i32 \"b\") \<##\> \"add the two input numbers\"
    ret result
@

Produces:

> define i32 @add_numbers(i32 %a, i32 %b) {
> entry:
>   ; Explanation: This block performs a simple addition
>   ; of two 32-bit integers
>   %1 = add i32 %a, %b  ; add the two input numbers
>   ret i32 %1
> }
-}
addNumbers :: IRBuilder ()
addNumbers =
  define i32 "add_numbers" [(i32, "a"), (i32, "b")] LExternal [] $ do
    beginBlock "entry"
    emitAnnotation
      ( commentBlock
          [ "Explanation: This block performs a simple addition"
          , "of two 32-bit integers"
          ]
      )
    result <- add i32 (OLocal i32 "a") (OLocal i32 "b") <##> "add the two input numbers"
    ret result

{- | Iterative factorial using @mdo@ for back-edge phi nodes.

The phi source operands @newAcc@ and @newN@ are Haskell bindings defined
in the @\"body\"@ block but referenced in the @\"loop\"@ phi nodes that
appear /earlier/ in the source. This forward reference requires @mdo@:

@
factorial :: IRBuilder ()
factorial = do
  declareVarArg \"printf\" i32 [ptr]
  emitGlobal (IRString LPrivate \".fmt\" \"fact(5) = %ld\\n\\0\")
  define i64 \"fact\" [(i64, \"n\")] LExternal [] $ mdo
    beginBlock \"entry\"
    br \"loop\"

    beginBlock \"loop\"
    accPhi <- phi i64 [(OConstant (CInt 64 1), \"entry\"), (newAcc, \"body\")]
    nPhi   <- phi i64 [(OLocal i64 \"n\",        \"entry\"), (newN,   \"body\")]
    cond   <- icmp ICmpSGt i64 nPhi (OConstant (CInt 64 0))
    condbr cond \"body\" \"exit\"

    beginBlock \"body\"
    newAcc <- mul i64 accPhi nPhi
    newN   <- sub i64 nPhi (OConstant (CInt 64 1))
    br \"loop\"

    beginBlock \"exit\"
    ret accPhi

  define i32 \"main\" [] LExternal [] $ do
    beginBlock \"entry\"
    result <- call NoTail i64 (OGlobal i64 \"fact\") [OConstant (CInt 64 5)]
    callVoidVarArg NoTail i32 [ptr] (OGlobal i32 \"printf\") [OGlobal ptr \".fmt\", result]
    ret (OConstant (CInt 32 0))
@

Produces:

> @.fmt = private constant [15 x i8] c"fact(5) = %ld\0a\00"
>
> declare i32 @printf(ptr, ...)
>
> define i64 @fact(i64 %n) {
> entry:
>   br label %loop
> loop:
>   %1 = phi i64 [ 1, %entry ], [ %4, %body ]
>   %2 = phi i64 [ %n, %entry ], [ %5, %body ]
>   %3 = icmp sgt i64 %2, 0
>   br i1 %3, label %body, label %exit
> body:
>   %4 = mul i64 %1, %2
>   %5 = sub i64 %2, 1
>   br label %loop
> exit:
>   ret i64 %1
> }
>
> define i32 @main() {
> entry:
>   %6 = call i64 @fact(i64 5)
>   call i32 (ptr, ...) @printf(ptr @.fmt, i64 %6)
>   ret i32 0
> }
-}
factorial :: IRBuilder ()
factorial = do
  declareVarArg "printf" i32 [ptr]
  emitGlobal (IRString LPrivate ".fmt" "fact(5) = %ld\n\0")
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
    callVoidVarArg NoTail i32 [ptr] (OGlobal i32 "printf") [OGlobal ptr ".fmt", result]
    ret (OConstant (CInt 32 0))
