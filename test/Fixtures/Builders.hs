{-# LANGUAGE OverloadedStrings #-}

module Fixtures.Builders (
  -- * Simple builders
  buildSimpleBlock,
  buildSimpleFunction,
  buildSimpleModule,
  buildBlockWithTerminator,
  buildInstructionBlock,

  -- * Complex builders
  buildMultiBlockFunction,
  buildFunctionWithAttributes,

  -- * Error scenario builders
  buildModuleWithDuplicateBlockNames,
  buildModuleWithInvalidBranchTarget,
)
where

import Data.Text (Text)
import LLVM.IRInstruction (IRInstruction (..))
import LLVM.IRModule (
  IRAttribute (..),
  IRBlock (..),
  IRBlockItem (..),
  IRFunction (..),
  IRLinkage (..),
  IRModule (..),
 )
import LLVM.IROperand (IRConstant (..), IROperand (..), IRTerminator (..))
import LLVM.IRType (IRName, IRType (..))

-- | Build a simple block with a return terminator
buildSimpleBlock :: IRName -> IRBlock
buildSimpleBlock label =
  IRBlock
    { blockLabel = label
    , blockItems = []
    , blockTerminator = IRet (Just (OConstant (CInt 32 0)))
    }

-- | Build a simple function with one entry block
buildSimpleFunction :: IRName -> IRFunction
buildSimpleFunction fname =
  IRFunction
    { functionName = fname
    , functionLinkage = LExternal
    , functionRetType = TInt 32
    , functionArgs = []
    , functionBlocks = [buildSimpleBlock "entry"]
    , functionAttributes = []
    }

-- | Build a simple module with one function
buildSimpleModule :: IRName -> IRModule
buildSimpleModule mname =
  IRModule
    { moduleName = mname
    , moduleDecls = []
    , moduleGlobals = []
    , moduleFunctions = [buildSimpleFunction "main"]
    }

-- | Build a block with a specific terminator
buildBlockWithTerminator :: IRName -> IRTerminator -> IRBlock
buildBlockWithTerminator label term =
  IRBlock
    { blockLabel = label
    , blockItems = []
    , blockTerminator = term
    }

-- | Build a block with an instruction and terminator
buildInstructionBlock :: IRName -> IRInstruction (Maybe Text) -> IRTerminator -> IRBlock
buildInstructionBlock label instr term =
  IRBlock
    { blockLabel = label
    , blockItems = [BlockInstr instr]
    , blockTerminator = term
    }

-- | Build a function with multiple blocks
buildMultiBlockFunction :: IRName -> [IRBlock] -> IRFunction
buildMultiBlockFunction fname blocks =
  IRFunction
    { functionName = fname
    , functionLinkage = LExternal
    , functionRetType = TInt 32
    , functionArgs = []
    , functionBlocks = blocks
    , functionAttributes = []
    }

-- | Build a function with attributes
buildFunctionWithAttributes :: IRName -> [IRAttribute] -> IRFunction
buildFunctionWithAttributes fname attrs =
  IRFunction
    { functionName = fname
    , functionLinkage = LExternal
    , functionRetType = TInt 32
    , functionArgs = []
    , functionBlocks = [buildSimpleBlock "entry"]
    , functionAttributes = attrs
    }

-- | Build a module with duplicate block names (for error testing)
buildModuleWithDuplicateBlockNames :: IRModule
buildModuleWithDuplicateBlockNames =
  let dupBlock = buildSimpleBlock "entry"
      func =
        IRFunction
          { functionName = "bad_func"
          , functionLinkage = LExternal
          , functionRetType = TInt 32
          , functionArgs = []
          , functionBlocks = [dupBlock, dupBlock]
          , functionAttributes = []
          }
   in IRModule
        { moduleName = "bad_module"
        , moduleDecls = []
        , moduleGlobals = []
        , moduleFunctions = [func]
        }

-- | Build a module with an invalid branch target
buildModuleWithInvalidBranchTarget :: IRModule
buildModuleWithInvalidBranchTarget =
  let block =
        IRBlock
          { blockLabel = "entry"
          , blockItems = []
          , blockTerminator = IBr "nonexistent"
          }
      func =
        IRFunction
          { functionName = "bad_branch_func"
          , functionLinkage = LExternal
          , functionRetType = TInt 32
          , functionArgs = []
          , functionBlocks = [block]
          , functionAttributes = []
          }
   in IRModule
        { moduleName = "bad_branch_module"
        , moduleDecls = []
        , moduleGlobals = []
        , moduleFunctions = [func]
        }
