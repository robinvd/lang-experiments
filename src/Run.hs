{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
module Run where

import           Prelude              hiding (lookup)

import           Bound
import           Control.Monad.Except
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Text.Megaparsec      (parse, parseErrorPretty')

import           Core
import           Emit
import           Interpreter
import           Parser
import           Rename
import           Type
import           Typechecker

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

liftMaybe :: Monad m => Maybe a -> ExceptT Err m a
liftMaybe m = case m of
                Just x  -> liftEither $ Right x
                Nothing -> liftEither $ Left $ RunError "result is a func"

total fileName = do
  p <- ExceptT (parseText fileName <$> T.readFile fileName)
  let renamed :: Core () Name
      (renamed, count) = rename p
      preludeTyped :: Core () (Name, Type)
      preludeTyped = (\x -> (x, fst $ lookup $ nameOrig x)) <$> renamed
  t <- liftEither $ inferExpr mempty preludeTyped
  liftIO $ print t
  liftIO $ putStrLn "---"
  e <- eval (snd . lookup . nameOrig . fst <$> t)
  eres <- liftMaybe $ closed e
  liftIO $ print (eres :: Core Scheme T.Text)
  llvmres <- llvm t count
  liftIO $ print llvmres



run :: String -> IO ()
run file = do
  t <- runExceptT $ total file
  case t of
    Left err -> print err
    Right _  -> putStrLn "succes"

llvm :: Core Scheme (Name,Scheme) -> Int -> ExceptT Err IO Int
llvm core count = do
  let ast = runEmit core count
  e <- liftIO $ do
    putStrLn "start llvm"
    ret <- withContext $ \c -> jit c $ \mcjit ->
      withModuleFromAST c ast $ \bc -> do
        withPassManager passes $ \pm -> do
          runPassManager pm bc
          writeLLVMAssemblyToFile (File "out") bc
          moduleLLVMAssembly bc >>= liftIO . B.putStrLn
          EE.withModuleInEngine mcjit bc $ \em -> do
            mf <- EE.getFunction em (AST.mkName "main")
            case mf of
              Just f  -> Right <$> haskFun (castFunPtr f :: FunPtr (IO Int))
              Nothing -> pure $ Left $ RunError "no main"
    putStrLn "done"
    return ret
  liftEither e

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultPassSetSpec
  { transforms = []
    -- [ AlwaysInline False
    -- , FunctionAttributes
    -- , FunctionInlining 5
    -- , PromoteMemoryToRegister
    -- , Reassociate
    -- , TailCallElimination
    -- , Sinking
    -- , InstructionCombining
    -- ]
  }
