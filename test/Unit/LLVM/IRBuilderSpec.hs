{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRBuilderSpec (spec) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Control.Monad.Except (runExceptT)
import LLVM.IRBuilder
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..))
import LLVM.IRBuilder.Environment (emptyIRBuilderEnv)
import LLVM.IRBuilder.Error (IRBuilderError)
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..))
import LLVM.IRModule (IRFunction (..), IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRType (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

runBuilder :: IRBuilder a -> IRBuilderEnv -> Either IRBuilderError (a, IRBuilderEnv)
runBuilder b env = runIdentity (runExceptT (runStateT (runIRBuilder b) env))

execBuilder :: IRBuilder a -> IRBuilderEnv -> IRBuilderEnv
execBuilder b env = case runBuilder b env of
  Right (_, e) -> e
  Left err -> error $ show err

testFB :: FunctionBuilder
testFB =
  FunctionBuilder
    { functionBuilderName = "test"
    , functionBuilderLinkage = LExternal
    , functionBuilderRetType = TInt 32
    , functionBuilderArgs = []
    , functionBuilderBlocks = []
    , functionBuilderAttributes = []
    }

spec :: Spec
spec = describe "LLVM.IRBuilder" $ do
  describe "beginBlock" $ do
    it "creates a new current block" $ do
      let env = execBuilder (beginBlock "entry") emptyIRBuilderEnv
      case builderEnvCurrentBlock env of
        Just bb -> blockBuilderLabel bb `shouldBe` "entry"
        Nothing -> expectationFailure "expected a current block"

    it "initializes block with empty items" $ do
      let env = execBuilder (beginBlock "entry") emptyIRBuilderEnv
      case builderEnvCurrentBlock env of
        Just bb -> blockBuilderItems bb `shouldBe` []
        Nothing -> expectationFailure "expected a current block"

    it "initializes block with no terminator" $ do
      let env = execBuilder (beginBlock "entry") emptyIRBuilderEnv
      case builderEnvCurrentBlock env of
        Just bb -> blockBuilderTerminator bb `shouldBe` Nothing
        Nothing -> expectationFailure "expected a current block"

  describe "setTerminator" $ do
    it "sets the terminator on the current block" $ do
      let env = execBuilder (beginBlock "entry" >> setTerminator (IBr "exit")) emptyIRBuilderEnv
      case builderEnvCurrentBlock env of
        Just bb -> blockBuilderTerminator bb `shouldBe` Just (IBr "exit")
        Nothing -> expectationFailure "expected a current block"

  describe "beginFunction / endFunction" $ do
    it "produces a function in the environment" $ do
      let env = execBuilder (beginFunction testFB >> beginBlock "entry" >> setTerminator (IRet (OConstant (CInt 32 0))) >> endFunction) emptyIRBuilderEnv
      length (builderEnvFunctions env) `shouldBe` 1

    it "records the function name" $ do
      let env = execBuilder (beginFunction testFB >> beginBlock "entry" >> setTerminator (IRet (OConstant (CInt 32 0))) >> endFunction) emptyIRBuilderEnv
      case builderEnvFunctions env of
        [f] -> functionName f `shouldBe` "test"
        _ -> expectationFailure "expected exactly one function"

    it "clears current function after endFunction" $ do
      let env = execBuilder (beginFunction testFB >> beginBlock "entry" >> setTerminator (IRet (OConstant (CInt 32 0))) >> endFunction) emptyIRBuilderEnv
      builderEnvCurrentFunction env `shouldBe` Nothing

  describe "buildModule / compileModule" $ do
    it "produces empty output for an empty module" $ do
      let output = compileModule "test" (pure ())
      output `shouldBe` ""
