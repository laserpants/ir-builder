{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO
import Examples.Factorial (factorialModule)
import LLVM.IRBuilder

main :: IO ()
main = Data.Text.IO.putStrLn (compileModule "factorial" factorialModule)
