{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
module Core where

import           Bound
-- import Control.Applicative
import           Control.Monad        (ap)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Deriving
import           Data.Functor.Classes
import           Data.List            (elemIndex)
import           Data.Text            (Text)

import           Type

type Func = [Lit] -> ExceptT Err IO Lit

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

data Core t a
  = Call t (Core t a) [Core t a]
  | Lit Lit
  | V a
  | Let t Int [t] [Scope Int (Core t) a] (Scope Int (Core t) a)
  | Lam t Int  (Scope Int (Core t) a)
  | Case t (Core t a) t [Alt (Core t) a]
  deriving (Traversable, Functor, Foldable)

instance Bifunctor Core where
  second = fmap
  first f = \case
    Call t a b -> Call (f t) (first f a) (map (first f) b)

type Expr' = Core ()

instance Applicative (Core t) where
  pure = V
  (<*>) = ap
instance Monad (Core t) where
  return = V
  Call t x y >>= f = Call t (x >>= f) (map (>>= f) y)
  Lit l >>= _ = Lit l
  V a >>= f = f a
  Let t i ts ls e >>= f = Let t i ts (map (>>>= f) ls) (e >>>= f)
  Lam t i e >>= f = Lam t i (e >>>= f)
  Case t e at alts >>= f = Case t (e >>= f) at (map (>>>= f) alts)


type Expr = Expr' Text

deriveEq1   ''Core
deriveOrd1  ''Core
deriveShow1  ''Core

instance (Eq t,Eq a) => Eq (Core t a) where (==) = eq1

instance (Ord t,Ord a) => Ord (Core t a) where compare = compare1

instance (Show t,Show a) => Show (Core t a) where showsPrec = showsPrec1


let_ :: Eq a => [(a,Expr' a)] -> Expr' a -> Expr' a
let_ [] b = b
let_ bs b = Let () (length bs) [] (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

lam :: Eq a => [a] -> Expr' a -> Expr' a
lam [] b = b
lam bs b = Lam () (length bs) (abstr b)
  where abstr = abstract (`elemIndex` bs)

alt :: Eq a => Either a Lit -> Expr' a -> Alt Expr' a
alt mn expr =
  case mn of
    Right l -> Alt (PLit l) $ Scope (F . V <$> expr)
    Left  n -> Alt PVar $ abstract1 n expr

