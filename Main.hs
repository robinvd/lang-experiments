{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Prelude hiding (lookup)

import qualified Control.Monad.State as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M

import Bound

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
  | Var a
  | Let a (Expr' a) (Expr' a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

-- instance Show a => Show (Expr' a) where
--   show = \case
--     Call a b -> show a ++ "(" ++ show b ++ ")"
--     Lit x -> "'" ++ show x ++ "'"
--     Var a -> show a
--     Let n val expr -> "let " ++ show n ++ " = " ++ show val ++ " in " ++ show expr
--     External _ -> "Fn"

type Expr = Expr' Text


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

parseArgs :: Parser a -> Parser [a]
parseArgs = parens . commaSep

parseExpr :: Parser Expr
parseExpr =
  do
    symbol "let"
    n <- lowIdentifier
    symbol "="
    val <- parseExpr
    symbol "in"
    next <- parseExpr
    return $ Let n val next
  <|> try (do
    -- n <- parseExpr
    n <- lowIdentifier
    args <- parseArgs parseExpr
    return $ Call (Var n) args)
  <|> Var <$> lowIdentifier
  <|> Lit <$> parseLit

-- prelude

data Fs
  = External ([Fs] -> Fs)
  | FLit Lit
type Env = M.Map Text Fs

prelude :: Env
prelude = M.fromList $ map (fmap External)
  [ ("add", \[a,b] -> add a b)]
    where
      -- add :: Bitcode -> Bitcode -> Bitcode
      add (FLit (Int a)) (FLit (Int b)) = FLit $ Int (a+b)
      add (FLit (Float a)) (FLit (Float b)) = FLit $ Float (a +b)
      add _ _ = error "not valid types"

-- eval

lookup :: Text -> S.StateT Env IO Fs
lookup n = do
  env <- S.get
  case M.lookup n env of
    Just e -> return e
    Nothing -> do
      -- S.liftIO $ print env
      error $ "lookup of " ++ show n ++ " failed"

eval :: Expr -> S.StateT Env IO Fs
eval = \case
  Call n args -> do
    mf <- eval n
    case mf of
      External fn -> fn <$> mapM eval args
      FLit _ -> error "not a function"
  Let n val expr -> do
    bcVal <- eval val
    S.modify $ M.insert n bcVal
    eval expr
  Var x -> lookup x
  Lit x -> return $ FLit x

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

  final <- S.evalStateT e prelude
  case final of
    FLit l -> print l
    External _ -> putStrLn "fn"

