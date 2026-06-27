{-# LANGUAGE OverloadedStrings #-}

module Examples.Comments (commentsModule) where

import LLVM.IR

{- | Build LLVM IR for a simple addition function with inline and block comments.

Demonstrates two annotation styles:

1. Block comments for explaining sections of code
2. Inline comments for explaining individual operations

Note: Inline comments (<##>) can only be used after instructions that emit block items.
Terminators (ret, br, etc.) do not emit block items, so <##> cannot be used with them.

Emits:

> define i32 @add_numbers(i32 %a, i32 %b) {
> entry:
>   ; Explanation: This block performs a simple addition
>   ; of two 32-bit integers
>   %result = add i32 %a, %b  ; add the two input numbers
>   ret i32 %result
> }
-}
commentsModule :: IRBuilder ()
commentsModule =
  define i32 "add_numbers" [(i32, "a"), (i32, "b")] LExternal [] $ do
    beginBlock "entry"

    -- Emit a block comment explaining what the next section does
    emitAnnotation (commentBlock ["Explanation: This block performs a simple addition", "of two 32-bit integers"])

    -- Perform addition with an inline comment
    result <- add i32 (OLocal i32 "a") (OLocal i32 "b") <##> "add the two input numbers"

    -- Return the result (note: inline comments cannot be attached to ret)
    ret result
