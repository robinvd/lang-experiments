{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Emit (runEmit, primitives) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           LLVM.AST                   hiding (function)
import Bound (instantiate, instantiate1)
import qualified LLVM.AST                   as AST
import LLVM.AST.Float
import qualified LLVM.AST.CallingConvention as CC
import           LLVM.AST.Constant as C
import           LLVM.AST.Global as G
import           LLVM.AST.Type hiding (ptr)
import qualified LLVM.AST.Type as AST.T
import LLVM.AST.AddrSpace
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.FloatingPointPredicate as F
import           LLVM.AST.Typed
import qualified LLVM.AST.Linkage as L
import           LLVM.IRBuilder hiding (call, double)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import Control.Monad.State hiding (void)
import GHC.Float
import qualified Data.ByteString.Short as Short
import Data.String
import Data.Monoid

-- import System.IO.Unsafe
-- import           Text.Pretty.Simple   (pPrint)

import qualified Core                       as Core
import qualified Type as T

toShort :: T.Text -> Short.ShortByteString
toShort = fromString . T.unpack

ptr x = PointerType x (AddrSpace 1)

call :: MonadIRBuilder m 
     => Operand 
     -> [(Operand, [A.ParameterAttribute])] 
     -> m Operand
call = callWith CC.Fast
callWith :: MonadIRBuilder m 
     => CC.CallingConvention
     -> Operand 
     -> [(Operand, [A.ParameterAttribute])] 
     -> m Operand
callWith cc fun args = do
  let instr = Call {
    AST.tailCallKind = Nothing
  , AST.callingConvention = cc
  , AST.returnAttributes = []
  , AST.function = Right fun
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }
  case typeOf fun of
      FunctionType r _ _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
        _        -> emitInstr r instr
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
        _        -> emitInstr r instr
      _ -> error "Cannot call non-function (Malformed AST)."

-- | Define and emit a (non-variadic) function definition
functionWithAttr
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [Either A.GroupID A.FunctionAttribute]
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m a)  -- ^ Function body builder
  -> m Operand
functionWithAttr label attr argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , G.callingConvention = CC.Fast
      , linkage     = L.Private
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      , G.functionAttributes = attr
      , garbageCollectorName = Just "statepoint-example"
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

instance MonadIRBuilder m => MonadIRBuilder (ModuleBuilderT m) where
  liftIRState = lift . liftIRState

instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m) where
  liftModuleState = lift . liftModuleState

newtype Supply = Supply Word

runEmit :: Core.Core T.Scheme (T.Name, T.Scheme) -> Int -> AST.Module
runEmit core count = flip evalState (Supply 0) $ do
  -- buildModuleT "main" $ do
  defs <- execModuleBuilderT emptyModuleBuilder $ do
    setup
    let newCore = (\(n, s) -> (pure . ConstantOperand . GlobalReference (ptr $ typeOf s) . mkName . T.unpack . T.nameOrig $ n)) <$> core

    functionWithAttr "userMain" [] [] (ptr i64) $ \_ -> convert newCore >>= ret
  return defaultModule
    { moduleDefinitions = defs
    , moduleName = "main"
    }

convertLit :: (MonadState Supply m, MonadModuleBuilder m, MonadIRBuilder m) 
           => Core.Lit -> m Operand
convertLit l = case l of
  Core.Int i -> do
    space <- malloc i64 -- alloca i64 Nothing 0
    store space 0 (ConstantOperand $ Int 64 $ toInteger i)
    return space
  Core.Float f -> do
    space <- malloc double
    store space 0 (ConstantOperand $ Float $ Double $ float2Double f)
    return space
  -- Core.Char c -> $ Int 8 $ toInteger $ fromEnum c
  _ -> error "not done yet"

cmpLit :: (MonadIRBuilder m) => Operand -> Operand -> m Operand
cmpLit a b = do
  x <- getLit a
  y <- getLit b
  -- assume the same type (typechecker)
  case typeOf x of
    IntegerType _ -> do
      icmp I.EQ x y
    FloatingPointType _ -> do
      fcmp F.OEQ x y
    _ -> error "type not supported in cmpLit"
  where
    getLit opr =
      case typeOf opr of
        PointerType t _ -> do
          load opr 0 >>= getLit
        IntegerType _ -> pure opr
        FloatingPointType _ -> pure opr
        _ -> error "not a lit in cmpLit"

convert :: ( MonadFix m, MonadState Supply m, MonadModuleBuilder m)
           -- , MonadIRBuilder f)
        => Core.Core T.Scheme (IRBuilderT m Operand) -> IRBuilderT m Operand
convert c = case c of
  Core.Call t n args -> do
    irn <- convert n
    irs <- mapM convert args
    call irn [(a,[]) | a <- irs]
  Core.Lit l -> convertLit l
  Core.V n -> n
  Core.Let t i ts ns sc se -> do
    -- let init :: Monad m 
    --          => [Operand]
    --          -> [Core.Core T.Scheme (IRBuilderT m Operand)]
    let init elem = 
          map (instantiate (pure . pure . (elem !! ))) sc

    rec res <- forM (zip (init res) ns) $ \(c,n) -> convert c `named` (toShort n)
      
    convert $ 
      -- unsafePerformIO (pPrint res) 
      -- `seq` unsafePerformIO (print $ res !! 0 == res !! 1)
      -- `seq` 
      instantiate (pure . pure . (res !!)) se
  Core.Case t predCore armT arms -> do
    pred <- convert predCore
    finalizer <- freshName "end"
    ends <- (convertAlt finalizer pred) arms
    emitBlockStart finalizer
    phi ends

  Core.Lam (T.Forall _ (T.TFunc argT retT)) i ns sc -> do
    currName <- liftIRState $ gets builderNameSuggestion
    n <- maybe freshGlobal freshGlobalName currName
    lift $ functionWithAttr
      n
      []
      [(typeOf x, ParameterName (toShort n)) | (x,n) <- zip argT ns]
      (typeOf retT)
      (\args -> convert (instantiate (\x -> Core.V . pure $ args !! x) sc) >>= ret)

  -- _ -> pure $ ConstantOperand $ Int 64 2

-- | create a test and br
--   make a new block
convertAlt :: (MonadFix m, MonadState Supply m, MonadModuleBuilder m)
           => Name
           -> Operand 
           -> [Core.Alt (Core.Core T.Scheme) (IRBuilderT m Operand)]
           -> IRBuilderT m [(Operand, Name)]
convertAlt _ _ [] = do
  str <- mkString "case not complete"
  puts str
  exit
  pure []
convertAlt final pred (a:as) = do
  let (Core.Alt p sc) = a

  case p of
    Core.PLit l -> do
      lOpr <- convertLit l
      b <- cmpLit pred lOpr
      trueName <- freshName "cond"
      falseName <- freshName "cond"
      condBr b trueName falseName
      emitBlockStart trueName
      val <- convert $ instantiate1 undefined sc
      br final
      emitBlockStart falseName
      ((val,trueName) :) <$> convertAlt final pred as
      
    Core.PVar -> do
      let c = instantiate1 (pure . pure $ pred) sc

      name <- freshName "cond"
      br name
      emitBlockStart name
      oper <- convert c
      br final
      pure [(oper, name)]

freshGlobal :: MonadState Supply m => m Name
freshGlobal = do
  Supply n <- get
  put $ Supply (n+1)
  return $ UnName n

freshGlobalName :: MonadState Supply m => Short.ShortByteString -> m Name
freshGlobalName addition = do
  Supply n <- get
  put $ Supply (n+1)
  return $ Name $ addition <> fromString (show n)

mkString :: (MonadState Supply m, MonadModuleBuilder m) => String -> m (Operand)
mkString str = do
  name <- freshGlobal
  let arrT = ArrayType (fromInteger $ toInteger $ length str) i8
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name = name
    , G.type' = arrT
    , isConstant = True
    , initializer = Just $ Array i8 $ map (Int 8 . toInteger . fromEnum) str
    }
  return $ 
    ConstantOperand $ 
      C.BitCast 
        (GlobalReference (AST.T.ptr arrT) $ name)
        (AST.T.ptr i8)

puts :: MonadIRBuilder m => Operand -> m Operand
puts str = do
  callWith CC.C (ConstantOperand $ GlobalReference (AST.T.ptr $ FunctionType VoidType [ptr i8] False) "puts")
    [(str, [])]

exit :: MonadIRBuilder m => m Operand
exit = callWith CC.C (ConstantOperand $ GlobalReference (FunctionType VoidType [] False) "exit") []

malloc :: MonadIRBuilder m => Type -> m Operand
malloc t = do
  let ft = ptr $ FunctionType (ptr i8) [i64] False
      size = ConstantOperand $ Int 64 8
      
  space <- callWith CC.C (ConstantOperand $ GlobalReference ft "alloc") [(size,[])]
  bitcast space (ptr t)

primitives :: (MonadState Supply m, MonadModuleBuilder m) => m ()
primitives = do
  extern "puts" [AST.T.ptr i8] void
  -- extern "alloc" [i64] (ptr i8)
  extern "initGC" [] void
  extern "exit" [] void
  extern "fflush" [AST.T.ptr i8] void

  emitDefn $ GlobalDefinition functionDefaults
    { name        = "alloc"
    , parameters  = ([Parameter i64 (mkName "") []], False)
    , returnType  = ptr i8
    , garbageCollectorName = Just "statepoint-example"
    }

  -- extern "sqrt" [float] float
  let binf name ty opr = functionWithAttr
        name
        [Right A.AlwaysInline]
        [(ptr ty, NoParameterName), (ptr ty, NoParameterName)] 
        (ptr ty) 
        $ \[a,b] -> do
          x <- load a 0
          y <- load b 0
          new <- opr x y
          space <- malloc ty
          store space 0 new
          ret space

  -- binf "add" i64 add
  binf "+" i64 add
  -- binf "sub" i64 sub
  binf "-" i64 sub
  -- binf "mul" i64 mul
  -- binf "div" i64 sdiv
  -- binf "rem" i64 urem
  -- binf "fadd" AST.T.double fadd
  -- binf "fsub" AST.T.double fsub
  -- binf "fmul" AST.T.double fmul
  -- binf "fdiv" AST.T.double fdiv
  -- binf "frem" AST.T.double frem
  return ()

setup :: (MonadState Supply m, MonadModuleBuilder m) => m ()
setup = do
  str <- mkString "runPoll"
  primitives
  function "gc.safepoint_poll" [] void $ \_ -> do
    puts str
    retVoid
    
  function "main" [] (i64) buildMain
  return ()

buildMain :: Monad m => [AST.Operand] -> IRBuilderT m ()
buildMain _ = do
  let toT t a = ptr $ FunctionType t a False
      toF n t ta = Right $ ConstantOperand $ GlobalReference (toT t ta) $ mkName n
      fnT = ptr $ FunctionType void [ptr i8] False
      exitT = ptr $ FunctionType void [] False
      puts = toF "puts" void [ptr i8]
      fflush = toF "fflush" void [AST.T.ptr i8]
      null = (ConstantOperand $ C.IntToPtr (Int 8 0) (AST.T.ptr i8), [])

  r <- call (ConstantOperand $ GlobalReference (toT void []) "initGC") []
  r <- call (ConstantOperand $ GlobalReference (toT (ptr i64) []) "userMain") []
  emitInstrVoid $ Call Nothing CC.C [] fflush [null] [] []
  ret =<< load r 0
