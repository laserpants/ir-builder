{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.SupplySpec (spec) where

import Control.Monad.State (runState)
import Control.Monad.Trans.Free (iterT)
import Data.List (nub)
import LLVM.IRBuilder (IRBuilder, unpackIRBuilder)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv)
import LLVM.IRBuilder.Supply (fresh, freshOperand)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

runBuilder :: IRBuilder a -> IRBuilderEnv -> (a, IRBuilderEnv)
runBuilder b env = runState (iterT interpretF (unpackIRBuilder b)) env
 where
  interpretF _ = error "unexpected effect in supply test"

evalBuilder :: IRBuilder a -> IRBuilderEnv -> a
evalBuilder b env = fst (runBuilder b env)

execBuilder :: IRBuilder a -> IRBuilderEnv -> IRBuilderEnv
execBuilder b env = snd (runBuilder b env)

spec :: Spec
spec = describe "LLVM.IRBuilder.Supply" $ do
  describe "fresh" $ do
    it "generates a name from the counter" $ do
      let name = evalBuilder fresh emptyIRBuilderEnv
      name `shouldBe` "1"

    it "increments the counter after each call" $ do
      let env = execBuilder fresh emptyIRBuilderEnv
      builderEnvFresh env `shouldBe` 1

    it "generates unique names for sequential calls" $ do
      let names = evalBuilder (sequence [fresh, fresh, fresh]) emptyIRBuilderEnv
      length (nub names) `shouldBe` 3

    it "generates sequentially numbered names" $ do
      let names = evalBuilder (sequence [fresh, fresh, fresh]) emptyIRBuilderEnv
      names `shouldBe` ["1", "2", "3"]

  describe "freshOperand" $ do
    it "creates an OLocal operand with the given type" $ do
      let op = evalBuilder (freshOperand (TInt 32)) emptyIRBuilderEnv
      case op of
        OLocal t _ -> t `shouldBe` TInt 32
        _ -> expectationFailure "expected OLocal"

    it "creates operands with unique names" $ do
      let ops = evalBuilder (sequence [freshOperand (TInt 32), freshOperand (TInt 32)]) emptyIRBuilderEnv
      let names = [n | OLocal _ n <- ops]
      length (nub names) `shouldBe` 2

    it "preserves the type in the operand" $ do
      let op = evalBuilder (freshOperand TFloat) emptyIRBuilderEnv
      case op of
        OLocal TFloat _ -> pure ()
        _ -> expectationFailure "expected OLocal TFloat"
