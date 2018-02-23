{-# LANGUAGE OverloadedStrings #-}
module Type where

import Data.Text (Text)

newtype TVar = TV Text
  deriving (Show, Eq, Ord)

data Type
  = Type Text
  | TFunc [Type] Type
  | TVar TVar
  deriving (Show, Eq, Ord)

int = Type "Int"
float = Type "Float"
unit = Type "Unit"
intf1 = TFunc [int] int
intf2 = TFunc [int, int] int
