{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
module Typechecker where

import Bound
import Control.Monad.Except
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (nub)

import Type
import Core

typeLit :: Lit -> Type
typeLit = \case
  Int _ -> int
  Float _ -> float

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


