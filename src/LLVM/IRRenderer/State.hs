{-# LANGUAGE StrictData #-}

module LLVM.IRRenderer.State (IRRendererState (..), emptyIRRendererState) where

data IRRendererState = IRRendererState
  deriving (Show, Eq, Ord)

emptyIRRendererState :: IRRendererState
emptyIRRendererState = IRRendererState
