{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRModuleSpec (spec) where

import Fixtures.Builders
import LLVM.IRInstruction (IRICmpCond (..), IRInstrOp (..), IRInstruction (..))
import LLVM.IRModule
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRName, IRType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy)

spec :: Spec
spec = describe "LLVM.IRModule" $ do
  describe "IRModule construction" $ do
    it "creates a simple module" $ do
      let m = buildSimpleModule "test"
      moduleName m `shouldBe` "test"
      length (moduleFunctions m) `shouldBe` 1

  describe "IRFunction construction" $ do
    it "creates a simple function" $ do
      let f = buildSimpleFunction "foo"
      functionName f `shouldBe` "foo"
      functionRetType f `shouldBe` TInt 32
      length (functionBlocks f) `shouldBe` 1

  describe "IRBlock construction" $ do
    it "creates a simple block" $ do
      let b = buildSimpleBlock "entry"
      blockLabel b `shouldBe` "entry"
      blockTerminator b `shouldBe` IRet (Just (OConstant (CInt 32 0)))

  describe "verifyModule" $ do
    it "accepts a valid simple module" $ do
      let m = buildSimpleModule "valid"
      verifyModule m `shouldBe` Right ()

    it "accepts a valid multi-block function" $ do
      let block1 = buildBlockWithTerminator "entry" (IBr "exit")
          block2 = buildBlockWithTerminator "exit" (IRet (Just (OConstant (CInt 32 0))))
          m =
            IRModule
              { moduleName = "multi"
              , moduleTypeDecls = []
              , moduleGlobals = []
              , moduleFunctions = [buildMultiBlockFunction "f" [block1, block2]]
              }
      verifyModule m `shouldBe` Right ()

    it "rejects duplicate block names" $ do
      let m = buildModuleWithDuplicateBlockNames
      verifyModule m `shouldSatisfy` isLeft

    it "rejects invalid branch target" $ do
      let m = buildModuleWithInvalidBranchTarget
      verifyModule m `shouldSatisfy` isLeft

    it "handles conditional branches with valid targets" $ do
      let block1 = buildBlockWithTerminator "entry" (ICondBr (OConstant (CInt 1 1)) "then" "else")
          block2 = buildBlockWithTerminator "then" (IRet (Just (OConstant (CInt 32 1))))
          block3 = buildBlockWithTerminator "else" (IRet (Just (OConstant (CInt 32 0))))
          m =
            IRModule
              { moduleName = "condbr"
              , moduleTypeDecls = []
              , moduleGlobals = []
              , moduleFunctions = [buildMultiBlockFunction "f" [block1, block2, block3]]
              }
      verifyModule m `shouldBe` Right ()

    it "rejects conditional branch with invalid 'then' target" $ do
      let block1 = buildBlockWithTerminator "entry" (ICondBr (OConstant (CInt 1 1)) "invalid" "else")
          block2 = buildBlockWithTerminator "else" (IRet (Just (OConstant (CInt 32 0))))
          m =
            IRModule
              { moduleName = "bad_condbr"
              , moduleTypeDecls = []
              , moduleGlobals = []
              , moduleFunctions = [buildMultiBlockFunction "f" [block1, block2]]
              }
      verifyModule m `shouldSatisfy` isLeft

    it "accepts switch with valid default" $ do
      let block1 = buildBlockWithTerminator "entry" (ISwitch (OConstant (CInt 32 1)) "default" [])
          block2 = buildBlockWithTerminator "default" (IRet (Just (OConstant (CInt 32 0))))
          m =
            IRModule
              { moduleName = "switch"
              , moduleTypeDecls = []
              , moduleGlobals = []
              , moduleFunctions = [buildMultiBlockFunction "f" [block1, block2]]
              }
      verifyModule m `shouldBe` Right ()

  describe "typeCheckModule" $ do
    it "passes a valid simple module" $
      typeCheckModule (buildSimpleModule "test") `shouldBe` Right ()

    it "catches binary op lhs type mismatch" $
      moduleWithInstr
        (IAdd (TInt 32) (OConstant (CInt 64 1)) (OConstant (CInt 32 2)))
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches binary op rhs type mismatch" $
      moduleWithInstr
        (IAdd (TInt 32) (OConstant (CInt 32 1)) (OConstant (CInt 64 2)))
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches binary op wrong result type" $
      moduleWithInstr
        (IAdd (TInt 32) (OConstant (CInt 32 1)) (OConstant (CInt 32 2)))
        (Just ("r", TInt 64))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches icmp result not i1" $
      moduleWithInstr
        (IICmp ICmpEq (TInt 32) (OConstant (CInt 32 0)) (OConstant (CInt 32 1)))
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "accepts icmp with correct i1 result" $
      moduleWithInstr
        (IICmp ICmpEq (TInt 32) (OConstant (CInt 32 0)) (OConstant (CInt 32 1)))
        (Just ("r", TInt 1))
        `shouldSatisfy` (not . isLeft . typeCheckModule)

    it "catches select condition not i1" $
      moduleWithInstr
        (ISelect (TInt 32) (OConstant (CInt 32 1)) (OConstant (CInt 32 2)) (OConstant (CInt 32 3)))
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches load with non-ptr pointer operand" $
      moduleWithInstr
        (ILoad (TInt 32) (OConstant (CInt 32 0)))
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches store with non-ptr address operand" $
      moduleWithInstr
        (IStore (OConstant (CInt 32 42)) (OConstant (CInt 32 0)))
        Nothing
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "catches phi incoming type mismatch" $
      moduleWithInstr
        (IPhi (TInt 32) [(OConstant (CInt 64 0), "entry")])
        (Just ("r", TInt 32))
        `shouldSatisfy` (isLeft . typeCheckModule)

    it "provides a descriptive error message with function and block name" $ do
      let m =
            moduleWithInstr
              (IAdd (TInt 32) (OConstant (CInt 64 1)) (OConstant (CInt 32 2)))
              (Just ("r", TInt 32))
      case typeCheckModule m of
        Right () -> expectationFailure "expected a type error"
        Left err -> do
          err `shouldContain` "function 'f'"
          err `shouldContain` "block 'entry'"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

moduleWithInstr :: IRInstrOp -> Maybe (IRName, IRType) -> IRModule
moduleWithInstr instr result =
  IRModule
    { moduleName = "typecheck_test"
    , moduleTypeDecls = []
    , moduleGlobals = []
    , moduleFunctions =
        [ IRFunction
            { functionName = "f"
            , functionLinkage = LExternal
            , functionRetType = TVoid
            , functionArgs = []
            , functionBlocks =
                [ IRBlock
                    { blockLabel = "entry"
                    , blockItems =
                        [ BlockInstr
                            IRInstruction
                              { instrResult = result
                              , instrOp = instr
                              , instrMetadata = Nothing
                              }
                        ]
                    , blockTerminator = IRet Nothing
                    }
                ]
            , functionAttributes = []
            }
        ]
    }
