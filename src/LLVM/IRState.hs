{-# LANGUAGE StrictData #-}

module LLVM.IRState (IRState (..), emptyIRState) where

data IRState = IRState
  deriving (Show, Eq, Ord)

emptyIRState :: IRState
emptyIRState = IRState
