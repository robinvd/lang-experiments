{-# LANGUAGE OverloadedStrings #-}
module Emit where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           LLVM.AST                   hiding (function)
import qualified LLVM.AST                   as AST
import qualified LLVM.AST.CallingConvention as CC
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.AST.Type
import           LLVM.IRBuilder

-- import           Codegen
import qualified Core                       as Core

runEmit :: Core.Expr' Text -> AST.Module
runEmit core = buildModule "main" test

str = Array i8 $ map (Int 8 . toInteger . fromEnum) "test\n"

test :: MonadModuleBuilder m => m ()
test = do
  extern "puts" [ptr i8] void
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name = "str"
    , LLVM.AST.Global.type' = ArrayType 5 i8
    , isConstant = True
    , initializer = Just str}
  function "main" [] void buildMain
  return ()

buildMain :: Monad m => [AST.Operand] -> IRBuilderT m ()
buildMain _ = do
  let fnT = ptr $ FunctionType void [ptr i8] False
      puts = Right $ ConstantOperand $ GlobalReference fnT $ mkName "puts"
      arg = (ConstantOperand $ GlobalReference (ptr $ ArrayType 5 i8) $ mkName "str", [])

  emitInstrVoid $
    Call Nothing CC.C [] puts [arg] [] []
