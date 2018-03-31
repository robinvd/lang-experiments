{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Typechecker where

import           Bound
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Control.Monad.Reader   as R
import qualified Control.Monad.State    as S
import           Data.Bifunctor
import           Data.List              (nub)
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Core
import           Type

typeLit :: Lit -> Type
typeLit = \case
  Int _ -> int
  Float _ -> float

type Subst = M.Map TVar Type
newtype TypeEnv = TypeEnv {types :: (M.Map Text Scheme)} deriving (Monoid)
type Constraint = (Type, Type)
type Unifier = (Subst, [Constraint])

newtype Unique = Unique {count :: Int}

type Infer a = (R.ReaderT
                  TypeEnv
                  (S.StateT Unique (Except Err))
                  a)

runInfer :: TypeEnv -> Infer a -> Either Err a
runInfer env m = runExcept $ S.evalStateT (R.runReaderT m env) (Unique 0)

inferExpr :: TypeEnv -> Core a (Name,Type) -> Either Err (Core Scheme (Name,Scheme))
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (core, ty, cs) -> case runSolve cs of
    Left err    -> Left err
    Right subst -> Right $
      bimap
        (closeOver . apply subst)
        (\(n,t) -> (n, closeOver $ apply subst t))
        core

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
  apply subst (Type t)    = Type t
  apply subst (TVar v)    = M.findWithDefault (TVar v) v subst
  apply subst (TFunc a r) = TFunc (map (apply subst) a) (apply subst r)

  ftv (Type _)    = S.empty
  ftv (TVar a)    = S.singleton a
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

-- instance Substitutable t => Substitutable (Core t t) where
--   apply subst = \case
--     Call t a b -> Call (apply subst t) a b
--     Lit l -> Lit l
--     V a -> V $ apply subst a
--     Let t i ts ls body ->
--       Let (apply subst t) i (map (apply subst) ls) (apply subst body)
--   ftv expr = undefined


letters :: [Text]
letters = fmap T.pack $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- S.get
  S.put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

freshName :: Infer (Name,Type)
freshName = do
  s <- S.get
  S.put s{count = count s + 1}
  let n = count s + 1

  return $ (Name n (letters !! n), TVar $ TV (letters !! n))

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TFunc a b) = concatMap fv a ++ fv b
    fv (Type _)    = []

    normtype (TFunc a b) = TFunc (map normtype a) (normtype b)
    normtype (Type a)   = Type a
    normtype (TVar a)   =
      case L.lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"

tupleToInner :: [(a,b)] -> ([a],[b])
tupleToInner = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])

trippleToInner :: [(a,b,c)] -> ([a],[b],[c])
trippleToInner = foldr (\(x,y,z) (xs,ys,zs) -> (x:xs, y:ys, z:zs)) ([],[],[])

applyScope f = Scope . f . unscope

infer :: Core a (Name, Type) -> Infer (Core Type (Name, Type), Type, [Constraint])
infer = \case
  Lit l -> pure (Lit l, typeLit l, [])
  V t -> pure (V t, snd t, [])
  Call _ n as -> do
    (e1, t1, c1) <- infer n
    x <- mapM infer as
    let e2 = map (\(a,_,_) -> a) x
        t2 = map (\(_,a,_) -> a) x
        c2 = concatMap (\(_,_,a) -> a) x
    tv <- fresh
    pure (Call tv e1 e2, tv, c1 ++ c2 ++ [(t1, TFunc t2 tv)])
  Lam _ i ns sc -> do
    tv <- fresh
    tvargs <- replicateM i freshName
    let e = instantiate (V . (tvargs !!)) sc

    (e, t, c) <- infer e
    let thisT = TFunc (map snd tvargs) t

    return (lam thisT ns tvargs e, thisT, c)
  Let _ i _ ns letsSc sc -> do
    tvis <- replicateM i freshName
    let lets = map (instantiate (V . (tvis !!))) letsSc
        expr = instantiate (V . (tvis !!)) sc

    (c, tls, lcs) <- trippleToInner <$> mapM infer lets
    (ce, te, tc) <- infer expr

    let newExpr = let_ te tls ns (zip tvis c) ce
    pure (newExpr ,te, concat lcs ++ tc)
  Case _ e _ alts -> do
    tv <- freshName
    (ne, et, ec) <- infer e
    (patExpr, patConstr) <- tupleToInner <$> mapM (inferAltPat et tv) alts

    pure (Case et ne (snd tv) patExpr, snd tv, concat patConstr ++ ec)
      where
        inferAltPat :: Type -> (Name, Type) -> Alt (Core a) (Name, Type)
                    -> Infer (Alt (Core Type) (Name,Type), [Constraint])
        inferAltPat patBase eBase (Alt pat sc) = do
          patTV <- case pat of
                     PLit l -> do
                       (n, _) <- freshName
                       return (n, typeLit l)
                     PVar   -> freshName
          let expr = instantiate1 (V patTV) sc

          (ne, t,c) <- infer expr
          let alt = Alt pat $ abstract1 patTV ne

          pure (alt , (c ++ [(patBase, snd patTV), (snd eBase,t)]))


-- | Constraint solver monad
type Solve a = ExceptT Err Identity a

unifies :: Type -> Type -> Solve Subst
unifies t1 t2                       | t1 == t2 = return mempty
unifies (TVar v) t                  = v `bind` t
unifies t (TVar v)                  = v `bind` t
unifies (TFunc a1 r1) (TFunc a2 r2) = unifyMany (r1:a1) (r2:a2)
unifies t1 t2                       = throwError $ TypeError t1 t2

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


