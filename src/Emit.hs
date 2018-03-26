{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Emit where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           LLVM.AST                   hiding (function)
import qualified LLVM.AST                   as AST
import qualified LLVM.AST.CallingConvention as CC
import           LLVM.AST.Constant as C
import           LLVM.AST.Global
import           LLVM.AST.Type
import           LLVM.IRBuilder
import LLVM.IRBuilder.Internal.SnocList
import Control.Monad.State hiding (void)

-- import           Codegen
import qualified Core                       as Core

newtype Supply = Supply Word

runEmit :: Core.Expr' Text -> AST.Module
runEmit core = buildModule "main" $ evalStateT (setup >> convert core) (Supply 0)

convert core = return ()

str = Array i8 $ map (Int 8 . toInteger . fromEnum) "test"

freshGlobal :: MonadState Supply m => m Name
freshGlobal = do
  Supply n <- get
  return $ UnName n

mkString :: (MonadState Supply m, MonadModuleBuilder m) => String -> m (Operand)
mkString str = do
  name <- freshGlobal
  let arrT = ArrayType (fromInteger $ toInteger $ length str) i8
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name = name
    , LLVM.AST.Global.type' = arrT
    , isConstant = True
    , initializer = Just $ Array i8 $ map (Int 8 . toInteger . fromEnum) str
    }
  return $ ConstantOperand $ GlobalReference (ptr arrT) $ name

mkPrelude :: MonadModuleBuilder m => m ()
mkPrelude = do
  extern "puts" [ptr i8] void
  extern "exit" [] void
  extern "fflush" [ptr i8] void
  return ()

setup :: (MonadState Supply m, MonadModuleBuilder m) => m ()
setup = do
  -- emitDefn $ GlobalDefinition globalVariableDefaults
  --   { name = "str"
  --   , LLVM.AST.Global.type' = ArrayType 5 i8
  --   , isConstant = True
  --   , initializer = Just str}
  str <- mkString "hello world\0"
  mkPrelude
  function "main" [] void $ buildMain str
  return ()

buildMain :: Monad m => AST.Operand -> [AST.Operand] -> IRBuilderT m ()
buildMain str _ = do
  let toT t a = ptr $ FunctionType t a False
      toF n t ta = Right $ ConstantOperand $ GlobalReference (toT t ta) $ mkName n
      fnT = ptr $ FunctionType void [ptr i8] False
      exitT = ptr $ FunctionType void [] False
      puts = toF "puts" void [ptr i8]
      fflush = toF "fflush" void [ptr i8]
      arg = (ConstantOperand $ GlobalReference (ptr $ ArrayType 5 i8) $ mkName "str", [])
      null = (ConstantOperand $ C.IntToPtr (Int 8 0) (ptr i8), [])

  emitInstrVoid $ Call Nothing CC.C [] puts [(str, [])] [] []
  -- emitInstrVoid $ Call Nothing CC.C [] exit [] [] []
  emitInstrVoid $ Call Nothing CC.C [] fflush [null] [] []
