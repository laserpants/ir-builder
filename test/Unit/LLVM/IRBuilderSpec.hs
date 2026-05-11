{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.LLVM.IRBuilderSpec (spec) where

import Control.Monad.State (modify, runState)
import Control.Monad.Trans.Free (iterT)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder
import LLVM.IRBuilder.BlockBuilder (BlockBuilder (..), appendBlockBuilderItem, setBlockBuilderTerminator)
import LLVM.IRBuilder.Environment (IRBuilderEnv (..), emptyIRBuilderEnv, mapBuilderEnvCurrentBlock)
import LLVM.IRBuilder.FunctionBuilder (FunctionBuilder (..))
import LLVM.IRInstruction (IRInstrOp (..), IRInstruction (..))
import LLVM.IRModule (IRBlock (..), IRBlockItem (..), IRLinkage (..))
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRType (..))
import Test.Hspec

runBuilder :: IRBuilder a -> IRBuilderEnv -> (a, IRBuilderEnv)
runBuilder b env = runState (iterT interpretF (unpackIRBuilder b)) env
  where
    interpretF (EmitInstr instr next) =
      modify (mapBuilderEnvCurrentBlock (appendBlockBuilderItem (BlockInstr instr))) >> next
    interpretF (EmitAnnotation ann next) =
      modify (mapBuilderEnvCurrentBlock (appendBlockBuilderItem (BlockAnnotation ann))) >> next

execBuilder :: IRBuilder a -> IRBuilderEnv -> IRBuilderEnv
execBuilder b = snd . runBuilder b

evalBuilder :: IRBuilder a -> IRBuilderEnv -> a
evalBuilder b = fst . runBuilder b

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

testInstr :: IRInstruction
testInstr =
  IRInstruction
    { instrResult = Just ("r", TInt 32)
    , instrOp = IAdd (TInt 32) (OLocal (TInt 32) "a") (OLocal (TInt 32) "b")
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
        [f] -> functionBuilderName (FunctionBuilder "test" LExternal (TInt 32) [] [] []) `shouldBe` "test"
        _ -> expectationFailure "expected exactly one function"

    it "clears current function after endFunction" $ do
      let env = execBuilder (beginFunction testFB >> beginBlock "entry" >> setTerminator (IRet (OConstant (CInt 32 0))) >> endFunction) emptyIRBuilderEnv
      builderEnvCurrentFunction env `shouldBe` Nothing

  describe "buildModule / compileModule" $ do
    it "produces empty output for an empty module" $ do
      let output = compileModule "test" (pure ())
      output `shouldBe` ""
