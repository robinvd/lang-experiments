{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
module Run where

import           Prelude              hiding (lookup)

import           Control.Monad.Except
import           Data.Monoid
import           Data.String          (fromString)
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy.IO    as TL
import qualified LLVM.AST             as AST
import           LLVM.Pretty          (ppllvm)
import           System.Exit
import           System.Process
import           Text.Megaparsec      (parseErrorPretty)
import           Text.Pretty.Simple

import           Core
import           Emit
import           Parser
import           Rename
import           Type
import           Typechecker

total :: String -> ExceptT Err IO ()
total fileName = do
  p <- ExceptT (parseText fileName <$> T.readFile fileName)
  let renamed :: Core () Name
      (renamed, baseCount) = rename p
      preludeTyped :: Core () (Name, Type)
      preludeTyped = (\x -> (x, lookupPrim $ nameOrig x)) <$> renamed
  t <- liftEither $ inferExpr mempty preludeTyped
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

spawn :: FilePath -> [String] -> ExceptT Err IO ()
spawn n args = do
  res <- liftIO $ waitForProcess =<< spawnProcess n args
  case res of
    ExitSuccess   -> return ()
    ExitFailure i -> throwError $ ExternalErr i n args ""

llvm :: String -> Core Scheme (Name,Scheme) -> Int -> ExceptT Err IO Int
llvm fileName core baseCount = do
  let baseFile = takeWhile (/= '.') fileName
      ast = (runEmit core baseCount)
        {AST.moduleSourceFileName = fromString fileName}
  liftIO $ TL.writeFile (baseFile ++ ".ast") $ pShowNoColor ast

  let llFile = baseFile ++ ".ll"
      passes =
        -- [ "-internalize"
        [ "-simplifycfg"
        , "-always-inline"
        , "-globaldce"
        , "-functionattrs"
        , "-tailcallelim"
        , "-strip-dead-debug-info"
        -- , "-place-backedge-safepoints-impl"
        , "-rewrite-statepoints-for-gc"
        ]
      outArg = ["-o", baseFile ++ ".bc"]
      includes =
        [ "lib/gc.c"
        , "lib/statepoint.c"
        , "lib/gcdef.ll"
        ]
      clangOutArg = ["-o", baseFile]
  liftIO $ do
    TL.writeFile llFile $
      "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"\n"
      <> "target triple = \"x86_64-unknown-linux-gnu\"\n"
      <> ppllvm ast
  spawn "opt" (passes ++ [llFile] ++ outArg)
  spawn "llc" [baseFile ++ ".bc"]
  liftIO $ T.appendFile (baseFile ++ ".s") ".globl __LLVM_StackMaps"
  spawn "clang" ([baseFile ++ ".s"] ++ includes ++ clangOutArg)

  res <- liftIO $ do
    (exitc, stdout, stderr) <-
      readCreateProcessWithExitCode (proc baseFile []) ""
    putStrLn stdout
    putStrLn stderr
    return $ case exitc of
                 ExitSuccess   -> 0
                 ExitFailure i -> i
  return res

