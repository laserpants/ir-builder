{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRRendererSpec (spec) where

import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Text (unpack)
import LLVM.IRInstruction
import LLVM.IRModule (IRAttribute (..), IRBlock (..), IRBlockItem (..), IRDecl (..), IRFunction (..), IRGlobal (..), IRLinkage (..), IRModule (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRRenderer (renderModule, runIRRenderer)
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

render :: IRModule -> String
render = unpack . runIRRenderer . renderModule

-- | Build a minimal module for testing
minimalModule :: IRModule
minimalModule =
  IRModule
    { moduleName = "test",
      moduleDecls = [],
      moduleGlobals = [],
      moduleFunctions = []
    }

-- | Build a module with a single trivial function
singleFuncModule :: IRModule
singleFuncModule =
  minimalModule
    { moduleFunctions =
        [ IRFunction
            { functionName = "main",
              functionLinkage = LExternal,
              functionRetType = TInt 32,
              functionArgs = [],
              functionBlocks =
                [ IRBlock
                    { blockLabel = "entry",
                      blockItems = [],
                      blockTerminator = IRet (Just (OConstant (CInt 32 0)))
                    }
                ],
              functionAttributes = []
            }
        ]
    }

spec :: Spec
spec = describe "LLVM.IRRenderer" $ do
  describe "renderModule" $ do
    it "renders an empty module without error" $ do
      let out = render minimalModule
      out `shouldBe` ""

    it "renders a module with a function" $ do
      let out = render singleFuncModule
      out `shouldContain` "define"
      out `shouldContain` "@main"
      out `shouldContain` "entry:"
      out `shouldContain` "ret i32"

    it "renders function return type" $ do
      let out = render singleFuncModule
      out `shouldContain` "i32"

    it "renders function arguments" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction
                      { functionName = "f",
                        functionLinkage = LExternal,
                        functionRetType = TInt 32,
                        functionArgs = [(TInt 32, "x"), (TFloat, "y")],
                        functionBlocks =
                          [ IRBlock "entry" [] (IRet (Just (OConstant (CInt 32 0))))
                          ],
                        functionAttributes = []
                      }
                  ]
              }
      let out = render m
      out `shouldContain` "i32 %x"
      out `shouldContain` "float %y"

    it "renders function attributes" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction
                      { functionName = "f",
                        functionLinkage = LExternal,
                        functionRetType = TVoid,
                        functionArgs = [],
                        functionBlocks =
                          [ IRBlock "entry" [] IUnreachable
                          ],
                        functionAttributes = [NoReturn, NoUnwind]
                      }
                  ]
              }
      let out = render m
      out `shouldContain` "noreturn"
      out `shouldContain` "nounwind"

  describe "renderModule - type declarations" $ do
    it "renders a type declaration" $ do
      let m =
            minimalModule
              { moduleDecls =
                  [ IRDecl {declName = "MyType", declType = TStruct [TInt 32, TFloat]}
                  ]
              }
      let out = render m
      out `shouldContain` "%MyType = type"
      out `shouldContain` "{ i32, float }"

  describe "renderModule - globals" $ do
    it "renders an extern declaration" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRExtern "printf" (TInt 32) []]
              }
      let out = render m
      out `shouldContain` "declare"
      out `shouldContain` "@printf"

    it "renders a constant global" $ do
      let m =
            minimalModule
              { moduleGlobals =
                  [ IRConstant LPrivate "kVal" (TInt 32) (CInt 32 42)
                  ]
              }
      let out = render m
      out `shouldContain` "@kVal"
      out `shouldContain` "constant"
      out `shouldContain` "42"

    it "renders a mutable global variable" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRVar LExternal "sample_tree_cell" TPtr (CNull TPtr)]
              }
      let out = render m
      out `shouldContain` "@sample_tree_cell"
      out `shouldContain` "global"
      out `shouldContain` "ptr null"

    it "mutable global uses 'global' not 'constant'" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRVar LExternal "g" TPtr (CNull TPtr)]
              }
      let out = render m
      out `shouldContain` "global"
      not ("constant" `isInfixOf` out) `shouldBe` True

    it "mutable global respects linkage" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRVar LPrivate "g" (TInt 32) (CInt 32 0)]
              }
      let out = render m
      out `shouldContain` "private"

    it "mutable global with special-char name renders quoted" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRVar LExternal "my%var" TPtr (CNull TPtr)]
              }
      let out = render m
      out `shouldContain` "@\"my%var\""

  describe "renderModule - floating-point constants" $ do
    it "renders a float constant as a 16-digit hex bit pattern" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRConstant LExternal "f" TFloat (CFloat 0.5)]
              }
      let out = render m
      out `shouldContain` "0x3FE0000000000000"

    it "renders a double constant as a 16-digit hex bit pattern" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRConstant LExternal "d" TDouble (CDouble 0.5)]
              }
      let out = render m
      out `shouldContain` "0x3FE0000000000000"

    it "renders a non-exact float using the widened double bit pattern" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRConstant LExternal "f" TFloat (CFloat 5.3)]
              }
      let out = render m
      out `shouldContain` "0x4015333340000000"

  describe "renderModule - instructions" $ do
    it "renders add instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32),
                instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b"),
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []
                  ]
              }
      let out = render m
      out `shouldContain` "add"
      out `shouldContain` "%r ="

    it "renders icmp instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("cmp", TInt 1),
                instrOp = IICmp ICmpEq (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b"),
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 1) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 1) "cmp")))] []
                  ]
              }
      let out = render m
      out `shouldContain` "icmp eq"

    it "renders store instruction (no result)" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing,
                instrOp = IStore (OLocal (TInt 32) "v") (OGlobal TPtr "g"),
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "store i32 %v, ptr @g"

    it "renders load instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32),
                instrOp = ILoad (TInt 32) (OGlobal TPtr "g"),
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []
                  ]
              }
      let out = render m
      out `shouldContain` "load i32, ptr @g"

    it "renders call instruction with typed arguments" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing,
                instrOp = ICall NoTail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"],
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "call i32 @puts(ptr %s)"

    it "renders tail call instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing,
                instrOp = ICall Tail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"],
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "tail call i32 @puts(ptr %s)"

    it "renders musttail call instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing,
                instrOp = ICall MustTail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"],
                instrMetadata = Nothing
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "musttail call i32 @puts(ptr %s)"

  describe "renderModule - terminators" $ do
    it "renders ret terminator" $ do
      let out = render singleFuncModule
      out `shouldContain` "ret i32"

    it "renders ret void terminator" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [] (IRet Nothing)] []
                  ]
              }
      let out = render m
      out `shouldContain` "ret void"

    it "renders br terminator" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction
                      "f"
                      LExternal
                      TVoid
                      []
                      [ IRBlock "entry" [] (IBr "exit"),
                        IRBlock "exit" [] IUnreachable
                      ]
                      []
                  ]
              }
      let out = render m
      out `shouldContain` "br label %exit"

    it "renders condbr terminator" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction
                      "f"
                      LExternal
                      TVoid
                      []
                      [ IRBlock "entry" [] (ICondBr (OLocal (TInt 1) "c") "then" "else"),
                        IRBlock "then" [] IUnreachable,
                        IRBlock "else" [] IUnreachable
                      ]
                      []
                  ]
              }
      let out = render m
      out `shouldContain` "br i1"
      out `shouldContain` "%then"
      out `shouldContain` "%else"

    it "renders unreachable terminator" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "unreachable"

  describe "renderModule - linkage" $ do
    it "renders internal linkage" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LInternal TVoid [] [IRBlock "entry" [] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "internal"

    it "renders private linkage" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRConstant LPrivate "k" (TInt 32) (CInt 32 1)]
              }
      let out = render m
      out `shouldContain` "private"

  describe "name quoting" $ do
    it "plain function name renders without quotes" $ do
      let out = render singleFuncModule
      out `shouldContain` "@main"

    it "function name with special char renders quoted" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "make_%Leaf" LExternal TPtr [] [IRBlock "entry" [] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "@\"make_%Leaf\""

    it "global with special char renders quoted" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRConstant LPrivate "my%const" (TInt 32) (CInt 32 1)]
              }
      let out = render m
      out `shouldContain` "@\"my%const\""

    it "extern with special char renders quoted" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRExtern "printf%ext" TVoid []]
              }
      let out = render m
      out `shouldContain` "@\"printf%ext\""

    it "function parameter with special char renders quoted" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 32) [(TInt 32, "x%1")] [IRBlock "entry" [] (IRet (Just (OConstant (CInt 32 0))))] []
                  ]
              }
      let out = render m
      out `shouldContain` "%\"x%1\""

    it "TNamed type with special char renders quoted" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TNamed "My%Type") [] [IRBlock "entry" [] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "%\"My%Type\""

    it "plain TNamed type renders without quotes" $ do
      let m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TNamed "MyType") [] [IRBlock "entry" [] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "%MyType"

  describe "renderModule - type declarations" $ do
    it "renders a named struct type declaration" $ do
      let m = minimalModule {moduleDecls = [IRDecl "Node" (TStruct [TInt 32, TPtr, TPtr])]}
      render m `shouldContain` "%Node = type { i32, ptr, ptr }"

    it "renders multiple type declarations" $ do
      let m = minimalModule {moduleDecls = [IRDecl "Foo" (TStruct [TInt 32]), IRDecl "Bar" (TStruct [TPtr])]}
      let out = render m
      out `shouldContain` "%Foo = type { i32 }"
      out `shouldContain` "%Bar = type { ptr }"

    it "type declarations appear before globals and functions" $ do
      let m =
            minimalModule
              { moduleDecls = [IRDecl "Node" (TStruct [TInt 32])],
                moduleGlobals = [IRExtern "printf" TVoid [TPtr]]
              }
      let out = render m
      let nodePos = length $ takeWhile (/= '%') out
          declPos = length $ takeWhile (not . isPrefixOf "declare") (tails out)
      nodePos `shouldBe` min nodePos declPos

  describe "renderModule - external declarations" $ do
    it "renders a declare statement" $ do
      let m = minimalModule {moduleGlobals = [IRExtern "printf" TVoid [TPtr]]}
      render m `shouldContain` "declare void @printf(ptr)"

    it "renders declare with multiple arg types" $ do
      let m = minimalModule {moduleGlobals = [IRExtern "memcpy" TPtr [TPtr, TPtr, TInt 64]]}
      render m `shouldContain` "declare ptr @memcpy(ptr, ptr, i64)"
