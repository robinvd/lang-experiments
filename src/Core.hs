{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Core where

import Bound
-- import Control.Applicative
import Control.Monad (ap)
import Data.Deriving
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Functor.Classes
import Control.Monad.Except

import Type

type Func = [Lit] -> ExceptT Err IO Lit
data Err
  = TypeError Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TVar Type
  | TypeText Text
  | NotFoundError Text
  | DublicateError Text
  | ParseError
  | RunError Text
  | InternalErr
  deriving (Show)


data Lit
  = Int Int
  | Float Float
  | Char Char
  | String Text
  | Unit
  deriving (Eq,Ord,Show,Read)

data Alt f a = Alt Pat (Scope () f a)
  deriving (Traversable, Functor, Foldable, Ord, Eq, Read)

data Pat
  = PVar
  | PLit Lit
  deriving (Show, Eq, Ord, Read)

instance Monad f => Eq1 (Alt f)
instance Monad f => Ord1 (Alt f)
instance Bound Alt where
  Alt p b >>>= f = Alt p (b >>>= f)
instance (Monad f, Show1 f) => Show1 (Alt f) where
  liftShowsPrec sp a d (Alt pat sc) cont = 
    "Alt (" ++ show pat ++ ") (" ++ liftShowsPrec sp a d sc (")" ++ cont)

data Expr' a
  = Call (Expr' a) [Expr' a]
  | Lit Lit
  | V a
  | Let Int [Scope Int Expr' a] (Scope Int Expr' a)
  | Lam Int (Scope Int Expr' a)
  | Case (Expr' a) [Alt Expr' a]
  deriving (Traversable, Functor, Foldable)

instance Applicative Expr' where
  pure = V
  (<*>) = ap
instance Monad Expr' where
  return = V
  Call x y >>= f = Call (x >>= f) (map (>>= f) y)
  Lit l >>= _ = Lit l
  V a >>= f = f a
  Let i ls e >>= f = Let i (map (>>>= f) ls) (e >>>= f)
  Lam i e >>= f = Lam i (e >>>= f)
  Case e alts >>= f = Case (e >>= f) (map (>>>= f) alts)


type Expr = Expr' Text

deriveEq1   ''Expr'
deriveOrd1  ''Expr'
deriveShow1  ''Expr'

instance Eq a => Eq (Expr' a) where (==) = eq1
instance Ord a => Ord (Expr' a) where compare = compare1
instance Show a => Show (Expr' a) where showsPrec = showsPrec1


let_ :: Eq a => [(a,Expr' a)] -> Expr' a -> Expr' a
let_ [] b = b
let_ bs b = Let (length bs) (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

lam :: Eq a => [a] -> Expr' a -> Expr' a
lam [] b = b
lam bs b = Lam (length bs) (abstr b)
  where abstr = abstract (`elemIndex` bs)

alt :: Eq a => Either a Lit -> Expr' a -> Alt Expr' a
alt mn expr =
  case mn of
    Right l -> Alt (PLit l) $ Scope (F . V <$> expr)
    Left  n -> Alt PVar $ abstract1 n expr

