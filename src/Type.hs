{-# LANGUAGE OverloadedStrings #-}
module Type where

import           Data.Text       (Text)
import           Data.Void
import           Text.Megaparsec

data Err
  = TypeError Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TVar Type
  | TypeText Text
  | NotFoundError Text
  | DuplicateError Text
  | ParseError (ParseError (Token Text) (Void))
  | RunError Text
  | InternalErr
  deriving (Show)

data Name = Name
  { nameId   :: Int
  , nameOrig :: Text
  } deriving (Show)

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
