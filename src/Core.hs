{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
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

altModScope f (Alt p sc) = Alt p (f sc)

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
  | Let t Int [t] [Text] [Scope Int (Core t) a] (Scope Int (Core t) a)
  | Lam t Int [Text] (Scope Int (Core t) a)
  | Case t (Core t a) t [Alt (Core t) a]
  deriving (Traversable, Functor, Foldable)

instance Bifunctor Core where
  second = fmap
  first :: (a -> b) -> Core a c -> Core b c
  first f = \case
    Call t a b -> Call (f t) (first f a) (map (first f) b)
    Lit l -> Lit l
    V a -> V a
    Let t i ts ns sc se ->
      Let (f t) i (map f ts) ns (map process sc) (process se)
    Lam t i ns sc ->
      Lam (f t) i ns $ process sc
    Case t e armT arms ->
      Case (f t) (first f e) (f armT) (map (altModScope process) arms)
    where
     process = Scope . bimap f (second (first f)) . unscope

type Expr' = Core ()

instance Applicative (Core t) where
  pure = V
  (<*>) = ap
instance Monad (Core t) where
  return = V
  Call t x y >>= f = Call t (x >>= f) (map (>>= f) y)
  Lit l >>= _ = Lit l
  V a >>= f = f a
  Let t i ns ts ls e >>= f = Let t i ns ts (map (>>>= f) ls) (e >>>= f)
  Lam t i ns e >>= f = Lam t i ns (e >>>= f)
  Case t e at alts >>= f = Case t (e >>= f) at (map (>>>= f) alts)


type Expr = Expr' Text

deriveEq1   ''Core
deriveOrd1  ''Core
deriveShow1  ''Core

instance (Eq t,Eq a) => Eq (Core t a) where (==) = eq1

instance (Ord t,Ord a) => Ord (Core t a) where compare = compare1

instance (Show t,Show a) => Show (Core t a) where showsPrec = showsPrec1


let_ :: Eq a => t -> [t] -> [Text] -> [(a,Core t a)] -> Core t a -> Core t a
let_ _ _ _ [] b = b
let_ t ts ns bs b = Let t (length bs) ts ns (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

lam :: Eq a => t -> [Text] -> [a] -> Core t a -> Core t a
lam _ _ [] b = b
lam t ns bs b = Lam t (length bs) ns (abstr b)
  where abstr = abstract (`elemIndex` bs)

alt :: Eq a => Either a Lit -> Expr' a -> Alt Expr' a
alt mn expr =
  case mn of
    Right l -> Alt (PLit l) $ Scope (F . V <$> expr)
    Left  n -> Alt PVar $ abstract1 n expr

