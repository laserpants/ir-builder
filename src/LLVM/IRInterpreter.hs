{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLVM.IRInterpreter (compileModule) where

import Common (Name)
import Control.Monad.State (MonadState, State, evalState, execState, modify)
import Control.Monad.Trans.Free (iterT)
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder (IRBuilder (runIRBuilder), IRBuilderEnv (..), IRBuilderF (..), emptyIRBuilderEnv, overBuilderEnvCurrentFunction)
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule (IRDecl, IRFunction (..), IRGlobal, IRModule (..), appendAnnotation, appendInstr)
import LLVM.IRState (IRState (..), emptyIRState)

newtype IRInterpreter a = IRInterpreter {runIRInterpreter :: State IRState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRState
    )

step :: IRBuilderF (State IRBuilderEnv a) -> State IRBuilderEnv a
step =
  \case
    EmitInstr instr next -> do
      emitInstruction instr
      next
    EmitAnnotation ann next -> do
      emitAnnotation ann
      next

emitInstruction :: IRInstruction -> State IRBuilderEnv ()
emitInstruction instr = modify $ overBuilderEnvCurrentFunction (fmap (appendInstr instr))

emitAnnotation :: IRAnnotation -> State IRBuilderEnv ()
emitAnnotation ann = modify $ overBuilderEnvCurrentFunction (fmap (appendAnnotation ann))

buildIR :: IRBuilder a -> State IRBuilderEnv a
buildIR = iterT step . runIRBuilder

renderModule :: IRModule -> IRInterpreter Text
renderModule IRModule{..} = do
  decls <- traverse renderDecl moduleDecls
  globs <- traverse renderGlobal moduleGlobals
  funs <- traverse renderFunction moduleFunctions
  pure $ Text.unlines $ concat [decls, globs, funs]

-- TODO
renderDecl :: IRDecl -> IRInterpreter Text
renderDecl = undefined

-- TODO
renderGlobal :: IRGlobal -> IRInterpreter Text
renderGlobal = undefined

-- TODO
renderFunction :: IRFunction -> IRInterpreter Text
renderFunction = undefined

finalizeModule :: Name -> IRBuilderEnv -> IRModule
finalizeModule name IRBuilderEnv{..} =
  IRModule
    { moduleName = name
    , moduleDecls = reverse builderEnvDecls
    , moduleGlobals = reverse builderEnvGlobals
    , moduleFunctions = finalizeFunctions IRBuilderEnv{..}
    }

finalizeFunctions :: IRBuilderEnv -> [IRFunction]
finalizeFunctions = undefined

buildModule :: Name -> IRBuilder a -> IRModule
buildModule name builder = finalizeModule name env
 where
  env = execState (buildIR builder) emptyIRBuilderEnv

runRenderer :: IRInterpreter Text -> Text
runRenderer interpreter = evalState (runIRInterpreter interpreter) emptyIRState

compileModule :: Name -> IRBuilder a -> Text
compileModule name = runRenderer . renderModule . buildModule name
