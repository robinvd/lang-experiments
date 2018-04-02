{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
module Run where

import           Prelude                       hiding (lookup)

import           Control.Monad.Except
import qualified Data.ByteString.Char8         as B8
import           Data.String                   (fromString)
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.IO             as TL
import           Foreign.Ptr                   (FunPtr, castFunPtr)
import qualified LLVM.AST                      as AST
import           LLVM.Context
import qualified LLVM.ExecutionEngine          as EE
import           LLVM.Module                   as Mod
import           LLVM.PassManager
import           LLVM.Target
import           LLVM.Transforms
import           System.Exit
import           System.Process
import           Text.Megaparsec               (parse, parseErrorPretty)
import           Text.Pretty.Simple

import           Core
import           Emit
import           Interpreter
import           Parser
import           Rename
import           Type
import           Typechecker

import qualified LLVM.Internal.FFI.PassManager as F

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

liftMaybe :: Monad m => Maybe a -> ExceptT Err m a
liftMaybe m = case m of
                Just x  -> liftEither $ Right x
                Nothing -> liftEither $ Left $ RunError "result is a func"

total :: String -> ExceptT Err IO ()
total fileName = do
  p <- ExceptT (parseText fileName <$> T.readFile fileName)
  let renamed :: Core () Name
      (renamed, baseCount) = rename p
      preludeTyped :: Core () (Name, Type)
      preludeTyped = (\x -> (x, lookupPrim $ nameOrig x)) <$> renamed
  t <- liftEither $ inferExpr mempty preludeTyped
  -- liftIO $ pPrint t
  -- liftIO $ putStrLn "---"
  -- e <- eval (snd . lookup . nameOrig . fst <$> t)
  -- eres <- liftMaybe $ closed e
  -- liftIO $ print (eres :: Core Scheme T.Text)
  llvmres <- llvm fileName t baseCount
  liftIO $ putStrLn $ "exit code: " ++ show llvmres

run :: String -> IO ()
run file = do
  t <- runExceptT $ total file
  case t of
    Left err -> case err of
                  ParseError pErr -> putStr $ parseErrorPretty pErr
                  _               -> print err
    Right _  -> putStrLn "succes"

llvm :: String -> Core Scheme (Name,Scheme) -> Int -> ExceptT Err IO Int
llvm fileName core baseCount = do
  let baseFile = takeWhile (/= '.') fileName
      ast = (runEmit core baseCount)
        {AST.moduleSourceFileName = fromString fileName}
  liftIO $ TL.writeFile (baseFile ++ ".ast") $ pShowNoColor ast
  liftIO $ putStrLn "start llvm"
  liftIO $
    withContext $ \c ->
    withHostTargetMachine $ \tm ->
    withModuleFromAST c ast $ \bc ->
    withPassManager (passes $ Just tm) $ \pm -> do
      F.addGlobalDeadCodeEliminationPass' pm
      runPassManager pm bc
      writeLLVMAssemblyToFile (File (baseFile ++ ".ll")) bc
      -- moduleLLVMAssembly bc >>= liftIO . B8.putStrLn
      writeTargetAssemblyToFile tm (File (baseFile ++ ".s")) bc
      writeObjectToFile tm (File (baseFile ++ ".so")) bc
  res <- liftIO $ do
    let files =
          [ baseFile ++ ".so"
          , "lib/gc.c"
          ]
    callCommand $ "clang " ++ unwords files ++ " -o " ++ baseFile
    (exitc, stdout, stderr) <-
      readCreateProcessWithExitCode (proc baseFile []) ""
    putStrLn stdout
    return $ case exitc of
                 ExitSuccess   -> 0
                 ExitFailure i -> i
  return res

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: Maybe TargetMachine -> PassSetSpec
passes tm =
  defaultPassSetSpec
    { targetMachine = tm
    , transforms = noPasses
    }

noPasses,allPasses,testPasses :: [Pass]
noPasses = []
allPasses =
  [ AlwaysInline True
  , InternalizeFunctions ["main"]
  , FunctionAttributes
  , PartialInlining
  , FunctionInlining 1
  , PromoteMemoryToRegister
  -- , Reassociate
  , TailCallElimination
  , Sinking
  , ArgumentPromotion

  , InstructionCombining
  , GlobalValueNumbering True

  , DeadCodeElimination

  , DeadInstructionElimination
  , DeadStoreElimination
  --
  , GlobalDeadCodeElimination
  ]
testPasses =
  [ AlwaysInline True
  , InternalizeFunctions ["main"]
  , FunctionAttributes
  -- , PartialInlining
  -- , FunctionInlining 1
  -- , PromoteMemoryToRegister
  -- , Reassociate
  , TailCallElimination
  -- , SimplifyLibCalls
  -- , Sinking
  -- , ArgumentPromotion

  , InstructionCombining
  , GlobalValueNumbering True
  , SimplifyControlFlowGraph

  -- , DeadCodeElimination

  -- , DeadInstructionElimination
  -- , DeadStoreElimination
  --
  , GlobalDeadCodeElimination
  ]
