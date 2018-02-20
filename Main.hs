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
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S

import Bound
import Bound.Scope
import Data.List hiding (lookup)
import Control.Applicative
import Data.Functor.Classes

import Data.Deriving

import Data.Void
import Text.Megaparsec hiding (match, count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- datas

data Lit
  = Int Int
  | Float Float
  | Char Char
  | String Text
  deriving (Eq,Ord,Show,Read)

data Alt f a = Alt Pat (Scope () f a)
  deriving (Traversable, Functor, Foldable, Ord, Eq, Read)

data Pat
  = PVar
  | PLit Lit
  -- | PWild
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

integer :: Parser Int
integer = lexeme L.decimal

hexInteger :: Parser Int
hexInteger = lexeme L.hexadecimal

parseFloat = lexeme L.float

charLit :: Parser Char
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
  try (Float <$> parseFloat)
  <|> Int <$> integer
  -- <|> Char <$> charLit

parseLam = do
  args <- parseArgs lowIdentifier
  symbol "->"
  e <- parseExpr
  return $ lam args e

parseCase = do
  symbol "case"
  e <- parseExpr
  symbol "of"
  alts <- commaSep $ do
    p <- parsePat
    symbol "->"
    body <- parseExpr
    return $ alt p body
  return $ Case e alts
  where
    parsePat =
      (Right <$> parseLit)
      <|> (Left <$> lowIdentifier)

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
  <|> parseCase
  <|> try (do
    -- n <- parseExpr
    n <- lowIdentifier
    args <- parseArgs parseExpr
    return $ Call (V n) args)
  <|> parseLam
  <|> V <$> lowIdentifier
  <|> Lit <$> parseLit

mainParse = between sc eof parseExpr


type Func = [Lit] -> Lit

-- eval

newtype TVar = TV Text
  deriving (Show, Eq, Ord)

data Type
  = Type Text
  | TFunc [Type] Type
  | TVar TVar
  deriving (Show, Eq, Ord)

int = Type "Int"
float = Type "Float"
intf1 = TFunc [int] int
intf2 = TFunc [int, int] int

typeLit :: Lit -> Type
typeLit = \case
  Int _ -> int
  Float _ -> float

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


data Scheme = Forall [TVar] Type deriving (Show)
type Subst = M.Map TVar Type
newtype TypeEnv = TypeEnv {types :: (M.Map Text Scheme)} deriving (Monoid)
type Constraint = (Type, Type)
type Unifier = (Subst, [Constraint])

newtype Unique = Unique {count :: Int}

type Infer a = (R.ReaderT
                  TypeEnv
                  (S.StateT Unique (Except Err))
                  a)

runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either Err (Type, [Constraint])
runInfer env m = runExcept $ S.evalStateT (R.runReaderT m env) (Unique 0)

inferExpr :: TypeEnv -> Expr' Type -> Either Err Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize (TypeEnv M.empty)

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
compose a b = fmap (apply b) a `M.union` b

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set TVar

instance Substitutable Type where
  apply subst (Type t) = Type t
  apply subst (TVar v) = M.findWithDefault (TVar v) v subst
  apply subst (TFunc a r) = TFunc (map (apply subst) a) (apply subst r)

  ftv (Type _) = S.empty
  ftv (TVar a) = S.singleton a
  ftv (TFunc a r) = foldr S.union S.empty (map ftv a) `S.union` ftv r

instance Substitutable Scheme where
  apply subst (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete subst as

  ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `S.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv xs = foldr (S.union . ftv) S.empty xs

instance Substitutable TypeEnv where
  apply subst (TypeEnv env) = TypeEnv $ fmap (apply subst) env
  ftv (TypeEnv env) = ftv $ M.elems env

letters :: [Text]
letters = fmap T.pack $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- S.get
  S.put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TFunc a b) = concatMap fv a ++ fv b
    fv (Type _)    = []

    normtype (TFunc a b) = TFunc (map normtype a) (normtype b)
    normtype (Type a)   = Type a
    normtype (TVar a)   =
      case L.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

tupleToInner :: [(a,b)] -> ([a],[b])
tupleToInner = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])

infer :: Expr' Type -> Infer (Type, [Constraint])
infer = \case
  Lit l -> pure (typeLit l, [])
  V t -> pure (t, [])
  Call n as -> do
    (t1, c1) <- infer n
    x <- mapM infer as
    let t2 = map fst x
        c2 = concatMap snd x
    tv <- fresh
    pure (tv, c1 ++ c2 ++ [(t1, TFunc t2 tv)])
  Lam i sc -> do
    tv <- fresh
    tvargs <- replicateM i fresh
    let e = instantiate (V . (tvargs !!)) sc

    (t, c) <- infer e
    return (TFunc tvargs t, c)
  Let i letsSc sc -> do
    tvis <- replicateM i fresh
    let lets = map (instantiate (V . (tvis !!))) letsSc
        expr = instantiate (V . (tvis !!)) sc

    (tls, lcs) <- tupleToInner <$> mapM infer lets
    (te, tc) <- infer expr

    pure (te, concat lcs ++ tc)
  Case e alts -> do
    tv <- fresh
    (et, ec) <- infer e
    patConstr <- mapM (inferAltPat et tv) alts
    pure (tv, concat patConstr ++ ec)
      where
        inferAltPat :: Type -> Type -> Alt Expr' Type -> Infer ([Constraint])
        inferAltPat patBase eBase (Alt pat sc) = do
          patTV <- case pat of
                         PLit l -> pure $ typeLit l
                         PVar -> fresh
          let expr = instantiate1 (V patTV) sc

          (t,c) <- infer expr
          pure (c ++ [(patBase, patTV), (eBase,t)])


-- | Constraint solver monad
type Solve a = ExceptT Err Identity a

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TFunc a1 r1) (TFunc a2 r2) = unifyMany (r1:a1) (r2:a2)
unifies t1 t2 = throwError $ TypeError t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `S.member` ftv t

runSolve :: [Constraint] -> Either Err Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (mempty, cs)

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

type Run = Either Err
eval :: Expr' Func -> Run (Expr' Func)
eval = \case
  Call n args -> do
    let toLit (Lit l) = l

    eArgs <- map toLit <$> mapM (eval) args
    case n of

       V t -> pure $ Lit $ t eArgs
       Lit _ -> throwError InternalErr
       x -> do
         n' <- eval n
         eval $ Call n' args

  Lit x -> pure $ Lit x
  V x -> pure $ V x
  Let i bs b -> eval (inst b)
    where es = map inst bs
          inst = instantiate (es !!)
  Lam _ l -> pure $ V (\args ->
    let eArgs = map Lit args
        inst = instantiate (eArgs !!)
        toLit (Right (Lit l)) = l
     in toLit (eval (inst l) ) )
  Case _ [] -> error "non-exhaustive case"
  Case e ((Alt pat sc):alts) ->
    eval e >>= \case
      Lit l -> case matches pat l of
                 False -> eval (Case (Lit l) alts)
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
  ]
    where
      -- litf :: Num a => (a -> a -> a) -> [Expr] -> Expr
      litf f [((Int a)), (Int b)] = Int $ f a b
      litf _ _ = error "not valid types"
      litf1 f [((Int b))] = Int $ f  b
      litf1 _ _ = error "not valid types"

lookup :: Text -> (Type, Func)
lookup n = do
  case M.lookup n prelude of
    Just e -> e
    Nothing -> error $ "lookup of " ++ show n ++ " failed"

main :: IO ()
main = run "test"

run file = do
  c <- T.readFile file
  p <- case parse mainParse file c of
         Right x -> return x
         Left x -> do
           putStrLn $ parseErrorPretty' c x
           error "failed parsing"

  print p
  let t = inferExpr mempty (fst . lookup <$> p) 
      e = t >> eval (snd . lookup <$> p)
  
  print t
  case e of
    Left err -> print err
    Right x -> case closed x of 
      Just x -> print (x :: Expr)
      Nothing -> putStrLn "end not a lit"
