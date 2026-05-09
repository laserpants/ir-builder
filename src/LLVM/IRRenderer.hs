{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLVM.IRRenderer (IRRenderer (..), runIRRenderer, renderModule) where

import Control.Monad.State (MonadState, State, evalState)
import Data.Text (Text)
import qualified Data.Text as Text
import LLVM.IRModule (IRDecl, IRFunction (..), IRGlobal, IRModule (..))
import LLVM.IRRenderer.State (IRRendererState (..), emptyIRRendererState)

newtype IRRenderer a = IRRenderer {unpackIRRenderer :: State IRRendererState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRRendererState
    )

runIRRenderer :: IRRenderer a -> a
runIRRenderer irRenderer = evalState (unpackIRRenderer irRenderer) emptyIRRendererState

renderModule :: IRModule -> IRRenderer Text
renderModule IRModule{moduleDecls, moduleGlobals, moduleFunctions} = do
  decls <- traverse renderDecl moduleDecls
  globs <- traverse renderGlobal moduleGlobals
  funs <- traverse renderFunction moduleFunctions
  pure $ Text.unlines $ concat [decls, globs, funs]

-- TODO
renderDecl :: IRDecl -> IRRenderer Text
renderDecl = undefined

-- TODO
renderGlobal :: IRGlobal -> IRRenderer Text
renderGlobal = undefined

-- TODO
renderFunction :: IRFunction -> IRRenderer Text
renderFunction = undefined
