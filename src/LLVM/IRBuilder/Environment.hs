{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LLVM.IRBuilder.Environment (
  IRBuilderEnv (..),
  emptyIRBuilderEnv,
  overBuilderEnvFresh,
  overBuilderEnvCurrentFunction,
  mapBuilderEnvCurrentFunction,
) where

import Common (Name)
import Data.Map.Strict (Map)
import LLVM.IRModule (IRBlock, IRDecl, IRFunction, IRGlobal)

data IRBuilderEnv = IRBuilderEnv
  { builderEnvFresh :: Int
  , builderEnvCurrentBlock :: Maybe IRBlock
  , builderEnvCurrentFunction :: Maybe IRFunction
  , builderEnvBlocks :: Map Name IRBlock
  , builderEnvFunctions :: Map Name IRFunction
  , builderEnvGlobals :: [IRGlobal]
  , builderEnvDecls :: [IRDecl]
  }
  deriving (Show, Eq, Ord)

emptyIRBuilderEnv :: IRBuilderEnv
emptyIRBuilderEnv =
  IRBuilderEnv
    { builderEnvFresh = 0
    , builderEnvCurrentBlock = Nothing
    , builderEnvCurrentFunction = Nothing
    , builderEnvBlocks = mempty
    , builderEnvFunctions = mempty
    , builderEnvGlobals = mempty
    , builderEnvDecls = mempty
    }

overBuilderEnvFresh :: (Int -> Int) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvFresh fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvFresh = fn builderEnvFresh
    , ..
    }

overBuilderEnvCurrentFunction :: (Maybe IRFunction -> Maybe IRFunction) -> IRBuilderEnv -> IRBuilderEnv
overBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fn builderEnvCurrentFunction
    , ..
    }

mapBuilderEnvCurrentFunction :: (IRFunction -> IRFunction) -> IRBuilderEnv -> IRBuilderEnv
mapBuilderEnvCurrentFunction fn IRBuilderEnv{..} =
  IRBuilderEnv
    { builderEnvCurrentFunction = fmap fn builderEnvCurrentFunction
    , ..
    }
