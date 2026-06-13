{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRModuleSpec (spec) where

import Fixtures.Builders
import LLVM.IRModule
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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
              , moduleDecls = []
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
              , moduleDecls = []
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
              , moduleDecls = []
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
              , moduleDecls = []
              , moduleGlobals = []
              , moduleFunctions = [buildMultiBlockFunction "f" [block1, block2]]
              }
      verifyModule m `shouldBe` Right ()

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
