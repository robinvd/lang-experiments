{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lower where

import Bound
import Control.Monad
import qualified Control.Monad.State as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Classes
-- import Data.Deriving

import qualified Core as Core
import Type

data Val f a
  = Lit Core.Lit
  | V a
  -- | Lam Int (Scope Int f a)
  | Call (f a) [f a]
  deriving (Traversable, Functor, Foldable, Show)

data Lower a
  = Let Int [Scope Int Lower a] (Scope Int Lower a)
  | If (Lower a) (Lower a) (Lower a)
  | Val (Val Lower a)
  -- | deref struct
  deriving (Traversable, Functor, Foldable)

instance Show1 Lower
instance Eq1 Lower
instance Ord1 Lower

data Global = Global
  { name :: Text
  , args :: [Text]
  , argsT :: [Type]
  , retT :: Type
  , body :: Lower Text
  , isPure :: Bool
  }

data Env = Env
  { count :: Int
  , globals :: [Global]}

fresh :: S.MonadState Env m => m Text
fresh = do
  n <- S.gets count
  S.modify (\x -> x {count = n+1})
  pure $ T.pack $ show n

addGlobal :: Global -> S.State Env ()
addGlobal g = S.modify $ \s -> s{globals = g:globals s}

toGlobal :: Text -> Core.Expr' Text -> S.State Env ()
toGlobal name x = do
  low <- lowerP x
  -- addGlobal $ Global name 
  undefined

lowerP :: Core.Expr' Text -> S.State Env (Lower Text)
lowerP = \case
  Core.Call n as -> do
    ne <- (lowerP n) 
    ase <- mapM lowerP as
    pure $ Val $ Call ne ase
  Core.Lit l -> pure $ Val $ Lit l
  Core.V a -> pure $ Val $ V a
  Core.Let i ls e -> do
    newV <- replicateM i fresh
    let lets = map (instantiate (Core.V . (newV !!))) ls
        expr = instantiate (Core.V . (newV !!)) e
    mapM (\(a,b) -> toGlobal a b) $ zip newV lets
    lowerP expr
