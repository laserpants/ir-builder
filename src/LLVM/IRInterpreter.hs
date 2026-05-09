{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLVM.IRInterpreter (IRInterpreter (..)) where

import Common (Name)
import Control.Monad.State (MonadState, State, modify)
import Control.Monad.Trans.Free (iterT)
import Data.Text (Text)
import LLVM.IRAnnotation (IRAnnotation (..))
import LLVM.IRBuilder (IRBuilder (runIRBuilder), IRBuilderEnv (..), IRBuilderF (..))
import LLVM.IRInstruction (IRInstruction)
import LLVM.IRModule (IRFunction (..), IRModule (..), appendInstr)
import LLVM.IRState (IRState (..))

newtype IRInterpreter a = IRInterpreter {runIRInterpreter :: State IRState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRState
    )

emitInstruction :: IRInstruction -> State IRBuilderEnv ()
emitInstruction instr =
  modify $
    \env ->
      env
        { builderEnvCurrentFunction =
            fmap appendInstr (builderEnvCurrentFunction env)
        }

emitAnnotation :: IRAnnotation -> State IRBuilderEnv a
emitAnnotation ann =
  undefined

step :: IRBuilderF (State IRBuilderEnv a) -> State IRBuilderEnv a
step =
  \case
    EmitInstr instr next -> do
      emitInstruction instr
      next
    EmitAnnotation ann next -> do
      emitAnnotation ann
      next

buildIR :: IRBuilder a -> State IRBuilderEnv a
buildIR = iterT step . runIRBuilder

renderModule :: IRModule -> IRInterpreter Text
renderModule irModule = pure "TODO"

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

--  decls <- traverse renderDecl (moduleDecls modl)
--  globs <- traverse renderGlobal (moduleGlobals modl)
--  funs  <- traverse renderFunction (moduleFunctions modl)
--
--  pure $
--    Text.unlines $
--      concat [decls, globs, funs]

-- buildModule :: IRBuilder a -> IRModule
-- buildModule m = finalizeModule $ execState (buildIR m) emptyBuilderEnv
--
-- compileModule :: IRBuilder a -> Text
-- compileModule =
--   runRenderer .
--   renderModule .
--   buildModule
