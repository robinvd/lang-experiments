{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Emit where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           LLVM.AST                   hiding (function)
import Bound (instantiate)
import qualified LLVM.AST                   as AST
import LLVM.AST.Float
import qualified LLVM.AST.CallingConvention as CC
import           LLVM.AST.Constant as C
import           LLVM.AST.Global as G
import           LLVM.AST.Type
import           LLVM.AST.Typed
import           LLVM.IRBuilder
import qualified LLVM.AST.Attribute as A
import LLVM.IRBuilder.Internal.SnocList
import Control.Monad.State hiding (void)

-- import           Codegen
import qualified Core                       as Core
import qualified Type as T

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
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      , G.functionAttributes = attr
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
runEmit core count = flip evalState (Supply 0) $ buildModuleT "main" $ do
    setup
    let newCore = (\(n, s) -> (pure . ConstantOperand . GlobalReference (ptr $ typeOf s) . mkName . T.unpack . T.nameOrig $ n)) <$> core

    function "userMain" [] (ptr i64) $ \_ -> convert newCore >>= ret

convertLit :: (MonadState Supply m, MonadModuleBuilder m, MonadIRBuilder m) 
           => Core.Lit -> m Operand
convertLit = \case
  Core.Int i -> do
    space <- malloc -- alloca i64 Nothing 0
    store space 0 (ConstantOperand $ Int 64 $ toInteger i)
    return space
  -- Core.Float f -> return $ Float $ Double f
  -- Core.Char c -> $ Int 8 $ toInteger $ fromEnum c
  _ -> error "not done yet"

convert :: ( MonadState Supply m, MonadModuleBuilder m)
           -- , MonadIRBuilder f)
        => Core.Core T.Scheme (IRBuilderT m Operand) -> IRBuilderT m Operand
convert = \case 
  Core.Call t n args -> do
    irn <- convert n
    irs <- mapM convert args
    call irn [(a,[]) | a <- irs]
  Core.Lit l -> convertLit l
  Core.V n -> n
  Core.Let t i ts sc se -> do
    -- todo mutual dependence in fns
    let f (n,t) = Core.V $ undefined
        argsE = map 
          (instantiate (map f (undefined) !!))
          sc

    res <- forM argsE $ \(core) -> do
      convert core
      
    convert $ instantiate (pure . pure . (res !!)) se
  Core.Lam (T.Forall _ (T.TFunc argT retT)) i sc -> do
    n <- freshGlobal
    lift $ functionWithAttr
      n
      []
      [(typeOf x, NoParameterName) | x <- argT]
      (typeOf retT)
      (\args -> convert (instantiate (\x -> Core.V . pure $ args !! x) sc) >>= ret)

  _ -> pure $ ConstantOperand $ Int 64 2



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
    , G.type' = arrT
    , isConstant = True
    , initializer = Just $ Array i8 $ map (Int 8 . toInteger . fromEnum) str
    }
  return $ ConstantOperand $ GlobalReference (ptr arrT) $ name

malloc :: MonadIRBuilder m => m Operand
malloc = do
  let ft = ptr $ FunctionType (ptr i8) [i64] False
      size = ConstantOperand $ Int 64 4
      
  space <- call (ConstantOperand $ GlobalReference ft "malloc") [(size,[])]
  bitcast space (ptr i64)

mkPrelude :: (MonadState Supply m, MonadModuleBuilder m) => m ()
mkPrelude = do
  extern "puts" [ptr i8] void
  extern "malloc" [i64] (ptr i8)
  extern "exit" [] void
  extern "fflush" [ptr i8] void
  functionWithAttr
    "add" 
    [Right A.AlwaysInline]
    [(ptr i64, NoParameterName), (ptr i64, NoParameterName)] 
    (ptr i64) 
    $ \[a,b] -> do
      x <- load a 0
      y <- load b 0
      sum <- add x y
      space <- malloc --alloca i64 Nothing 0
      store space 0 sum
      ret space
  return ()

setup :: (MonadState Supply m, MonadModuleBuilder m) => m ()
setup = do
  mkPrelude
  -- str <- mkString "hello world\0"
  function "main" [] (i64) $ buildMain undefined
  return ()

buildMain :: Monad m => AST.Operand -> [AST.Operand] -> IRBuilderT m ()
buildMain str _ = do
  let toT t a = ptr $ FunctionType t a False
      toF n t ta = Right $ ConstantOperand $ GlobalReference (toT t ta) $ mkName n
      fnT = ptr $ FunctionType void [ptr i8] False
      exitT = ptr $ FunctionType void [] False
      puts = toF "puts" void [ptr i8]
      fflush = toF "fflush" void [ptr i8]
      null = (ConstantOperand $ C.IntToPtr (Int 8 0) (ptr i8), [])

  -- emitInstrVoid $ Call Nothing CC.C [] puts [(str, [])] [] []
  r <- call (ConstantOperand $ GlobalReference (toT (ptr i64) []) "userMain") []
  emitInstrVoid $ Call Nothing CC.C [] fflush [null] [] []
  ret =<< load r 0
