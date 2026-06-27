{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRRendererSpec (spec) where

import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Text (unpack)
import LLVM.IRInstruction
import LLVM.IRModule (IRAttribute (..), IRBlock (..), IRBlockItem (..), IRFunction (..), IRGlobal (..), IRLinkage (..), IRModule (..), IRTypeDecl (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRRenderer (renderModule)
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

render :: IRModule -> String
render = unpack . renderModule

-- | Build a minimal module for testing
minimalModule :: IRModule
minimalModule =
  IRModule
    { moduleName = "test"
    , moduleTypeDecls = []
    , moduleGlobals = []
    , moduleFunctions = []
    }

-- | Build a module with a single trivial function
singleFuncModule :: IRModule
singleFuncModule =
  minimalModule
    { moduleFunctions =
        [ IRFunction
            { functionName = "main"
            , functionLinkage = LExternal
            , functionRetType = TInt 32
            , functionArgs = []
            , functionBlocks =
                [ IRBlock
                    { blockLabel = "entry"
                    , blockItems = []
                    , blockTerminator = IRet (Just (OConstant (CInt 32 0)))
                    }
                ]
            , functionAttributes = []
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
                      { functionName = "f"
                      , functionLinkage = LExternal
                      , functionRetType = TInt 32
                      , functionArgs = [(TInt 32, "x"), (TFloat, "y")]
                      , functionBlocks =
                          [ IRBlock "entry" [] (IRet (Just (OConstant (CInt 32 0))))
                          ]
                      , functionAttributes = []
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
                      { functionName = "f"
                      , functionLinkage = LExternal
                      , functionRetType = TVoid
                      , functionArgs = []
                      , functionBlocks =
                          [ IRBlock "entry" [] IUnreachable
                          ]
                      , functionAttributes = [NoReturn, NoUnwind]
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
              { moduleTypeDecls =
                  [ IRTypeDecl{typeDeclName = "MyType", typeDeclType = TStruct [TInt 32, TFloat]}
                  ]
              }
      let out = render m
      out `shouldContain` "%MyType = type"
      out `shouldContain` "{ i32, float }"

  describe "renderModule - globals" $ do
    it "renders an extern declaration" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRExtern "printf" (TInt 32) [] False]
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
              { instrResult = Just ("r", TInt 32)
              , instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b")
              , instrMetadata = Nothing
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
              { instrResult = Just ("cmp", TInt 1)
              , instrOp = IICmp ICmpEq (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b")
              , instrMetadata = Nothing
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
              { instrResult = Nothing
              , instrOp = IStore (OLocal (TInt 32) "v") (OGlobal TPtr "g")
              , instrMetadata = Nothing
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
              { instrResult = Just ("r", TInt 32)
              , instrOp = ILoad (TInt 32) (OGlobal TPtr "g")
              , instrMetadata = Nothing
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
              { instrResult = Nothing
              , instrOp = ICall NoTail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"]
              , instrMetadata = Nothing
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
              { instrResult = Nothing
              , instrOp = ICall Tail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"]
              , instrMetadata = Nothing
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
              { instrResult = Nothing
              , instrOp = ICall MustTail (TInt 32) (OGlobal (TInt 32) "puts") [OLocal TPtr "s"]
              , instrMetadata = Nothing
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
                      [ IRBlock "entry" [] (IBr "exit")
                      , IRBlock "exit" [] IUnreachable
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
                      [ IRBlock "entry" [] (ICondBr (OLocal (TInt 1) "c") "then" "else")
                      , IRBlock "then" [] IUnreachable
                      , IRBlock "else" [] IUnreachable
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
              { moduleGlobals = [IRExtern "printf%ext" TVoid [] False]
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
      let m = minimalModule{moduleTypeDecls = [IRTypeDecl "Node" (TStruct [TInt 32, TPtr, TPtr])]}
      render m `shouldContain` "%Node = type { i32, ptr, ptr }"

    it "renders multiple type declarations" $ do
      let m = minimalModule{moduleTypeDecls = [IRTypeDecl "Foo" (TStruct [TInt 32]), IRTypeDecl "Bar" (TStruct [TPtr])]}
      let out = render m
      out `shouldContain` "%Foo = type { i32 }"
      out `shouldContain` "%Bar = type { ptr }"

    it "type declarations appear before globals and functions" $ do
      let m =
            minimalModule
              { moduleTypeDecls = [IRTypeDecl "Node" (TStruct [TInt 32])]
              , moduleGlobals = [IRExtern "printf" TVoid [TPtr] False]
              }
      let out = render m
      let nodePos = length $ takeWhile (/= '%') out
          declPos = length $ takeWhile (not . isPrefixOf "declare") (tails out)
      nodePos `shouldBe` min nodePos declPos

  describe "renderModule - external declarations" $ do
    it "renders a declare statement" $ do
      let m = minimalModule{moduleGlobals = [IRExtern "printf" TVoid [TPtr] False]}
      render m `shouldContain` "declare void @printf(ptr)"

    it "renders declare with multiple arg types" $ do
      let m = minimalModule{moduleGlobals = [IRExtern "memcpy" TPtr [TPtr, TPtr, TInt 64] False]}
      render m `shouldContain` "declare ptr @memcpy(ptr, ptr, i64)"

    it "renders a variadic declare with fixed args" $ do
      let m = minimalModule{moduleGlobals = [IRExtern "printf" (TInt 32) [TPtr] True]}
      render m `shouldContain` "declare i32 @printf(ptr, ...)"

    it "renders a variadic declare with no fixed args" $ do
      let m = minimalModule{moduleGlobals = [IRExtern "varonly" TVoid [] True]}
      render m `shouldContain` "declare void @varonly(...)"

  describe "renderModule - aggregate and vector instructions" $ do
    it "renders extractvalue" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32)
              , instrOp = IExtractValue (OLocal (TStruct [TInt 32, TInt 64]) "s") [0]
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []]}
      render m `shouldContain` "extractvalue { i32, i64 } %s, 0"

    it "renders insertvalue" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TStruct [TInt 32, TInt 64])
              , instrOp = IInsertValue (OLocal (TStruct [TInt 32, TInt 64]) "s") (OLocal (TInt 32) "v") [0]
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TStruct [TInt 32, TInt 64]) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TStruct [TInt 32, TInt 64]) "r")))] []]}
      render m `shouldContain` "insertvalue { i32, i64 } %s, i32 %v, 0"

    it "renders extractelement" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32)
              , instrOp = IExtractElement (OLocal (TVector 4 (TInt 32)) "v") (OLocal (TInt 32) "i")
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []]}
      render m `shouldContain` "extractelement <4 x i32> %v, i32 %i"

    it "renders insertelement" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TVector 4 (TInt 32))
              , instrOp = IInsertElement (OLocal (TVector 4 (TInt 32)) "v") (OLocal (TInt 32) "e") (OLocal (TInt 32) "i")
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TVector 4 (TInt 32)) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TVector 4 (TInt 32)) "r")))] []]}
      render m `shouldContain` "insertelement <4 x i32> %v, i32 %e, i32 %i"

    it "renders shufflevector" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TVector 4 (TInt 32))
              , instrOp = IShuffleVector (OLocal (TVector 4 (TInt 32)) "a") (OLocal (TVector 4 (TInt 32)) "b") [0, 2, 1, 3]
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TVector 4 (TInt 32)) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TVector 4 (TInt 32)) "r")))] []]}
      render m `shouldContain` "shufflevector <4 x i32> %a, <4 x i32> %b, <4 x i32> <i32 0, i32 2, i32 1, i32 3>"

  describe "renderModule - atomic instructions" $ do
    it "renders atomicrmw" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32)
              , instrOp = IAtomicRMW SeqCst ARMWAdd (OLocal TPtr "p") (OLocal (TInt 32) "v")
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []]}
      render m `shouldContain` "atomicrmw add ptr %p, i32 %v seq_cst"

    it "renders cmpxchg" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TStruct [TInt 32, TInt 1])
              , instrOp = ICmpXchg False SeqCst Monotonic (OLocal TPtr "p") (OLocal (TInt 32) "cmp") (OLocal (TInt 32) "new")
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TStruct [TInt 32, TInt 1]) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TStruct [TInt 32, TInt 1]) "r")))] []]}
      render m `shouldContain` "cmpxchg ptr %p, i32 %cmp, i32 %new seq_cst monotonic"

    it "renders fence" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing
              , instrOp = IFence AcqRel
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []]}
      render m `shouldContain` "fence acq_rel"

    it "renders freeze" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32)
              , instrOp = IFreeze (OLocal (TInt 32) "v")
              , instrMetadata = Nothing
              }
          m = minimalModule{moduleFunctions = [IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (Just (OLocal (TInt 32) "r")))] []]}
      render m `shouldContain` "freeze i32 %v"
