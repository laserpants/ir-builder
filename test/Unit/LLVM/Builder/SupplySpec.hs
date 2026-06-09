{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.Builder.SupplySpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Data.List (nub)
import LLVM.IRBuilder (IRBuilder (..))
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv)
import LLVM.IRBuilder.Error (IRBuilderError)
import LLVM.IRBuilder.Supply (fresh, freshLabel, freshOperand)
import LLVM.IROperand (IROperand (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

runBuilder :: IRBuilder a -> IRBuilderEnv -> Either IRBuilderError (a, IRBuilderEnv)
runBuilder b env = runIdentity (runExceptT (runStateT (runIRBuilder b) env))

evalBuilder :: IRBuilder a -> IRBuilderEnv -> a
evalBuilder b env = case runBuilder b env of
  Right (a, _) -> a
  Left err -> error $ show err

execBuilder :: IRBuilder a -> IRBuilderEnv -> IRBuilderEnv
execBuilder b env = case runBuilder b env of
  Right (_, e) -> e
  Left err -> error $ show err

spec :: Spec
spec = describe "LLVM.IRBuilder.Supply" $ do
  describe "fresh" $ do
    it "generates a name from the counter" $ do
      let name = evalBuilder fresh emptyIRBuilderEnv
      name `shouldBe` "1"

    it "increments the counter after each call" $ do
      let env = execBuilder fresh emptyIRBuilderEnv
      builderEnvFreshReg env `shouldBe` 1

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

  describe "freshLabel" $ do
    it "generates a label with the hint as a prefix" $ do
      let label = evalBuilder (freshLabel "loop") emptyIRBuilderEnv
      label `shouldBe` "loop.1"

    it "increments the fresh label counter" $ do
      let env = execBuilder (freshLabel "loop") emptyIRBuilderEnv
      builderEnvFreshLabel env `shouldBe` 1

    it "does not increment the fresh register counter" $ do
      let env = execBuilder (freshLabel "loop") emptyIRBuilderEnv
      builderEnvFreshReg env `shouldBe` 0

    it "generates unique labels for sequential calls with the same hint" $ do
      let labels = evalBuilder (sequence [freshLabel "loop", freshLabel "loop"]) emptyIRBuilderEnv
      labels `shouldBe` ["loop.1", "loop.2"]

    it "generates unique labels for sequential calls with different hints" $ do
      let labels = evalBuilder (sequence [freshLabel "loop", freshLabel "body"]) emptyIRBuilderEnv
      labels `shouldBe` ["loop.1", "body.2"]
