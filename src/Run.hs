{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
module Run where

import           Prelude              hiding (lookup)

import           Bound
import           Control.Monad.Except
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Text.Megaparsec      (parse, parseErrorPretty')

import           Core
import           Interpreter
import           Parser
import           Typechecker

import           Emit

import           Data.Int
import           Data.Word
import           Foreign.Ptr          (FunPtr, castFunPtr)

import           Control.Monad.Except

import qualified LLVM.AST             as AST
import           LLVM.CodeModel
import           LLVM.Context
import           LLVM.Module          as Mod
import           LLVM.Target

import           LLVM.Analysis
import           LLVM.PassManager
import           LLVM.Transforms

import qualified LLVM.ExecutionEngine as EE

import           LLVM.Module

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

run :: String -> IO ()
run file = do
  c <- T.readFile file
  p <- case parse mainParse file c of
         Right x -> return x
         Left x -> do
           putStrLn $ parseErrorPretty' c x
           error "failed parsing"

  -- print p
  let t = ExceptT $ pure $ inferExpr mempty (fst . lookup <$> p)
      e = t >> eval (snd . lookup <$> p)

  -- print t
  runExceptT e >>= \case
    Left err -> print err
    Right x -> case closed x of
      Just x  -> print (x :: Expr)
      Nothing -> putStrLn "end not a lit"

  let ast = runEmit undefined

  putStrLn "start llvm"
  withContext $ \c -> jit c $ \mcjit ->
    withModuleFromAST c ast $ \bc ->
      EE.withModuleInEngine mcjit bc $ \em -> do
        mf <- EE.getFunction em (AST.mkName "main")
        case mf of
          Just f  -> haskFun (castFunPtr f :: FunPtr (IO Int))
          Nothing -> error "no main"
  putStrLn "done"


jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
