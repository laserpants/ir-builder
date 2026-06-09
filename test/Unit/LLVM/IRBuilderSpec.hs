{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRBuilderSpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
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

evalBuilder :: IRBuilder a -> IRBuilderEnv -> a
evalBuilder b env = case runBuilder b env of
  Right (a, _) -> a
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

  describe "block" $ do
    it "returns a name with the hint as a prefix" $ do
      let label = evalBuilder (block "loop") emptyIRBuilderEnv
      label `shouldBe` "loop.1"

    it "creates a current block whose label matches the returned name" $ do
      let env = execBuilder (block "loop") emptyIRBuilderEnv
      case builderEnvCurrentBlock env of
        Just bb -> blockBuilderLabel bb `shouldBe` "loop.1"
        Nothing -> expectationFailure "expected a current block"

    it "two calls with the same hint produce distinct labels" $ do
      let labels = evalBuilder (do l1 <- block "loop"; setTerminator (IBr l1); l2 <- block "loop"; pure [l1, l2]) emptyIRBuilderEnv
      labels `shouldBe` ["loop.1", "loop.2"]

    it "does not increment the fresh register counter" $ do
      let env = execBuilder (block "loop") emptyIRBuilderEnv
      builderEnvFreshReg env `shouldBe` 0

  describe "IRBuilderT transformer" $ do
    it "can run in the IO monad" $ do
      result <- runExceptT (runStateT (runIRBuilderT (pure 42 :: IRBuilderT IO Int)) emptyIRBuilderEnv)
      case result of
        Right (val, _) -> val `shouldBe` 42
        Left err -> expectationFailure $ "Builder failed: " ++ show err

    it "lift propagates values from the base monad" $ do
      let action = lift (Just 100) :: IRBuilderT Maybe Int
      let result = runStateT (runIRBuilderT action) emptyIRBuilderEnv
      case runExceptT result of
        Just (Right (val, _)) -> val `shouldBe` 100
        _ -> expectationFailure "Expected Just (Right (100, _))"

    it "liftIO works when base monad is IO" $ do
      result <- runExceptT (runStateT (runIRBuilderT (liftIO (pure 999) :: IRBuilderT IO Int)) emptyIRBuilderEnv)
      case result of
        Right (val, _) -> val `shouldBe` 999
        Left err -> expectationFailure $ "Builder failed: " ++ show err
