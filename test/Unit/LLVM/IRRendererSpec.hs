{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRRendererSpec (spec) where

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
    { moduleName = "test"
    , moduleDecls = []
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
                    , blockTerminator = IRet (OConstant (CInt 32 0))
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
      out `shouldContain` "ret"

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
                          [ IRBlock "entry" [] (IRet (OConstant (CInt 32 0)))
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
              { moduleDecls =
                  [ IRDecl{declName = "MyType", declType = TStruct [TInt 32, TFloat]}
                  ]
              }
      let out = render m
      out `shouldContain` "%MyType = type"
      out `shouldContain` "{ i32, float }"

  describe "renderModule - globals" $ do
    it "renders an extern declaration" $ do
      let m =
            minimalModule
              { moduleGlobals = [IRExtern "printf" (TInt 32)]
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

  describe "renderModule - instructions" $ do
    it "renders add instruction" $ do
      let instr =
            IRInstruction
              { instrResult = Just ("r", TInt 32)
              , instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b")
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 32) [] [IRBlock "entry" [BlockInstr instr] (IRet (OLocal (TInt 32) "r"))] []
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
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal (TInt 1) [] [IRBlock "entry" [BlockInstr instr] (IRet (OLocal (TInt 1) "cmp"))] []
                  ]
              }
      let out = render m
      out `shouldContain` "icmp eq"

    it "renders store instruction (no result)" $ do
      let instr =
            IRInstruction
              { instrResult = Nothing
              , instrOp = IStore (OLocal (TInt 32) "v") (OGlobal (TPtr (TInt 32)) "g")
              }
          m =
            minimalModule
              { moduleFunctions =
                  [ IRFunction "f" LExternal TVoid [] [IRBlock "entry" [BlockInstr instr] IUnreachable] []
                  ]
              }
      let out = render m
      out `shouldContain` "store"

  describe "renderModule - terminators" $ do
    it "renders ret terminator" $ do
      let out = render singleFuncModule
      out `shouldContain` "ret"

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
