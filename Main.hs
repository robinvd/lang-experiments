{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Prelude hiding (lookup)

import qualified Control.Monad.State as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M

import Bound
import Data.List hiding (lookup)
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Data.Functor.Classes

import Data.Deriving

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

-- datas

type Identifier = Text

data Lit
  = Int Int
  | Float Float
  | Char Char
  | String Text
  deriving (Eq,Ord,Show,Read)
  -- deriving (Eq)
data Expr' a
  = Call (Expr' a) [Expr' a]
  | Lit Lit
  | V a
  | Let [Scope Int Expr' a] (Scope Int Expr' a)
  | Lam (Scope Int Expr' a)
  -- deriving (Show, Show1, Eq,Ord,Functor,Foldable,Traversable)
  deriving (Traversable, Functor, Foldable)


instance Applicative Expr' where
  pure = V
  (<*>) = ap
instance Monad Expr' where
  return = V
  Lit l >>= f = Lit l
  V a >>= f = f a
  Call x y >>= f = Call (x >>= f) (map (>>= f) y)
  Lam e   >>= f = Lam (e >>>= f)


type Expr = Expr' Text

deriveEq1   ''Expr'
deriveOrd1  ''Expr'
deriveRead1 ''Expr'
deriveShow1 ''Expr'

instance Eq a => Eq (Expr' a) where (==) = eq1
instance Ord a => Ord (Expr' a) where compare = compare1
instance Show a => Show (Expr' a) where showsPrec = showsPrec1
instance Read a => Read (Expr' a) where readsPrec = readsPrec1


let_ :: Eq a => [(a,Expr' a)] -> Expr' a -> Expr' a
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

lam :: Eq a => [a] -> Expr' a -> Expr' a
lam [] b = b
lam bs b = Lam (abstr b)
  where abstr = abstract (`elemIndex` bs)

-- lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

hexInteger = lexeme L.hexadecimal

float = lexeme L.float

charLit = lexeme L.charLiteral

identifier, capIdentifier, lowIdentifier :: Parser Text
(identifier, capIdentifier, lowIdentifier) =
  (ident p, ident upP, ident lowP)
  where
    ident f = T.pack <$> (lexeme . try) f
    p       = (:) <$> letterChar <*> many alphaNumChar
    lowP    = (:) <$> lowerChar <*> many alphaNumChar
    upP     = (:) <$> upperChar <*> many alphaNumChar

commaSep = flip sepBy (symbol ",")

-- parser

parseLit =
  Int . fromInteger <$> integer
  <|> Float <$> float
  <|> Char <$> charLit

parseLam = do
  args <- parseArgs lowIdentifier
  symbol "->"
  e <- parseExpr
  return $ lam args e

parseArgs :: Parser a -> Parser [a]
parseArgs = parens . commaSep


parseExpr :: Parser Expr
parseExpr =
  do
    symbol "let"
    xs <- commaSep $ do
      n <- lowIdentifier
      symbol "="
      val <- parseExpr
      return (n, val)
    symbol "in"
    next <- parseExpr
    return $ let_ xs next
  <|> try (do
    -- n <- parseExpr
    n <- lowIdentifier
    args <- parseArgs parseExpr
    return $ Call (V n) args)
  <|> parseLam
  <|> V <$> lowIdentifier
  <|> Lit <$> parseLit

-- prelude

data Fs
  = External ([Fs] -> Fs)
  | FLit Lit
type Env = M.Map Text ([Expr] -> Expr)

prelude :: Env
prelude = M.fromList $ 

  [ ("add", litf (+) (+))
  , ("mult", litf (*) (*))
  , ("neg", litf1 (negate) (negate))
  ]
    where
      -- litf :: Num a => (a -> a -> a) -> [Expr] -> Expr
      litf f _ [(Lit (Int a)), (Lit (Int b))] = Lit $ Int $ f a b
      litf _ f [(Lit (Float a)), (Lit (Float b))] = Lit $ Float $ f a b
      litf _ _ _ = error "not valid types"
      litf1 f _ [(Lit (Int b))] = Lit $ Int $ f  b
      litf1 _ f [(Lit (Float b))] = Lit $ Float $ f  b
      litf1 _ _ _ = error "not valid types"

-- eval

lookup :: Text -> [Expr] -> Expr
lookup n = do
  case M.lookup n prelude of
    Just e -> e
    Nothing -> error $ "lookup of " ++ show n ++ " failed"

-- whnf :: Expr -> Expr
-- whnf = \case
  -- Call f a -> do
  --   x <- whnf f
  --   case x of
  --     Lam b -> whnf (instantiate1 a b)
  --     f' -> App f' a
  -- x -> x

eval :: Expr -> Expr
eval = \case
  Call n args ->
    let eA = eval n
        eArgs = map eval args
     in case eA of

      V t -> (lookup t) eArgs
      Lam l -> 
        let inst = instantiate (args !!)
        in eval (inst l)
      Lit _ -> error "not a function"

  Lit x -> Lit x
  V x -> V x
  Let bs b -> eval (inst b)
    where es = map inst bs
          inst = instantiate (es !!)
  Lam e -> Lam e

main :: IO ()
main = run "test"

run file = do
  c <- T.readFile file
  p <- case parse parseExpr file c of
         Right x -> return x
         Left x -> do
           putStrLn $ parseErrorPretty' c x
           error "failed parsing"

  print p
  let e = eval p

  print e

  -- final <- S.evalStateT e prelude
  -- case final of
  --   FLit l -> print l
  --   External _ -> putStrLn "fn"

