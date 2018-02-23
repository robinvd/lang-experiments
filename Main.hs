{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Prelude hiding (lookup)
import Control.Monad.Except
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Control.Monad.Identity
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S

import Bound
import Data.List hiding (lookup)
import Control.Applicative
import Data.Functor.Classes

import Data.Deriving

import Data.Void
import Text.Megaparsec hiding (match, count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Run

main :: IO ()
main = run "test"


