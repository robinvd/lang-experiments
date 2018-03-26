{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Map as M
import Bound

import Core
import Type

type Run = ExceptT Err IO
eval :: Expr' Func -> Run (Expr' Func)
eval = \case
  Call t n args -> do
    let toLit (Lit l) = l

    eArgs <- map toLit <$> mapM (eval) args
    case n of

       V t -> Lit <$> t eArgs
       Lit _ -> throwError InternalErr
       x -> do
         n' <- eval n
         eval $ Call t n' args

  Lit x -> pure $ Lit x
  V x -> pure $ V x
  Let _ i _ bs b -> eval (inst b)
    where es = map inst bs
          inst = instantiate (es !!)
  Lam _ _ l -> pure $ V $ (\args ->
    let eArgs = map Lit args
        inst = instantiate (eArgs !!)
        newE = inst l
        toLit (Lit l) = l
     in toLit <$> (eval (newE)))
  Case _ _ _ [] -> error "non-exhaustive case"
  Case et e at ((Alt pat sc):alts) ->
    eval e >>= \case
      Lit l -> case matches pat l of
                 False -> eval (Case et (Lit l) at alts)
                 _ -> eval $ instantiate1 (Lit l) sc
      _ -> error "e in case not a lit"

matches :: Pat -> Lit -> Bool
matches (PLit l) lit = l == lit
matches _ _ = True

-- prelude

prelude :: M.Map Text (Type, Func)
prelude = M.fromList $ 

  [ ("add", (intf2,litf (+)))
  , ("mult", (intf2, litf (*)))
  , ("neg", (intf1, litf1 (negate)))
  , ("prInt", (TFunc [int] unit, prInt))
  ]
    where
      litf :: (Int -> Int -> Int) -> Func
      litf f [Int a, Int b] = pure $ Int $ f a b
      litf _ _ = error "not valid types"
      litf1 :: (Int -> Int) -> Func
      litf1 f [Int b] = pure $ Int $ f b
      litf1 _ _ = error "not valid types"
      prInt :: Func
      prInt [Int b] = pure Unit
      prInt _ = error "not valid types"

lookup :: Text -> (Type, Func)
lookup n = do
  case M.lookup n prelude of
    Just e -> e
    Nothing -> error $ "lookup of " ++ show n ++ " failed"
