{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Terminator.ConstructorsSpec (spec) where

import Control.Monad.State (runState)
import Control.Monad.Trans.Free (iterT)
import LLVM.IRBuilder (IRBuilder, beginBlock, setTerminator, unpackIRBuilder)
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..))
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv)
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRTerminator.Constructors
import LLVM.IRType (IRType (..))
import Test.Hspec

execBuilder :: IRBuilder a -> IRBuilderEnv -> IRBuilderEnv
execBuilder b env = snd $ runState (iterT interpretF (unpackIRBuilder b)) env
  where
    interpretF _ = error "unexpected effect"

currentTerminator :: IRBuilderEnv -> Maybe IRTerminator
currentTerminator env = blockBuilderTerminator =<< builderEnvCurrentBlock env

withBlock :: IRBuilder a -> IRBuilderEnv -> Maybe IRTerminator
withBlock action = currentTerminator . execBuilder (beginBlock "entry" >> action)

spec :: Spec
spec = describe "LLVM.IRTerminator.Constructors" $ do
  describe "ret" $
    it "sets IRet terminator" $ do
      let op = OConstant (CInt 32 0)
      withBlock (ret op) emptyIRBuilderEnv `shouldBe` Just (IRet op)

  describe "br" $
    it "sets IBr terminator" $
      withBlock (br "target") emptyIRBuilderEnv `shouldBe` Just (IBr "target")

  describe "condbr" $
    it "sets ICondBr terminator" $ do
      let cond = OConstant (CInt 1 1)
      withBlock (condbr cond "then" "else") emptyIRBuilderEnv
        `shouldBe` Just (ICondBr cond "then" "else")

  describe "switch" $ do
    it "sets ISwitch terminator with no cases" $ do
      let op = OConstant (CInt 32 0)
      withBlock (switch op "default" []) emptyIRBuilderEnv
        `shouldBe` Just (ISwitch op "default" [])

    it "sets ISwitch terminator with cases" $ do
      let op = OConstant (CInt 32 0)
          cases = [(CInt 32 1, "case1"), (CInt 32 2, "case2")]
      withBlock (switch op "default" cases) emptyIRBuilderEnv
        `shouldBe` Just (ISwitch op "default" cases)

  describe "unreachable" $
    it "sets IUnreachable terminator" $
      withBlock unreachable emptyIRBuilderEnv `shouldBe` Just IUnreachable
