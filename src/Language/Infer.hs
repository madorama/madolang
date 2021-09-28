{-# LANGUAGE TemplateHaskell #-}

module Language.Infer where

import           Control.Arrow              (left, (+++))
import           Control.Lens               hiding ((|>))
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Maybe
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Madlib.Operator

import           Language.Syntax
import           Language.Type

data Scheme
  = Scheme [Id] Type
  deriving (Show, Eq)

newtype TypeEnv =
  TypeEnv (M.Map Id Scheme)
  deriving (Show, Eq)

extend :: TypeEnv -> (Id, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) =
  TypeEnv $ M.insert x s env

emptyTypeEnv :: TypeEnv
emptyTypeEnv =
  TypeEnv M.empty

type Subst =
  M.Map Id Type

class Types a where
  apply :: Subst -> a -> a
  free :: a -> S.Set Id

instance Types Type where
  apply s = \case
    t@(TVar x) -> fromMaybe t $ M.lookup x s

    t          -> t

  free = \case
    TVar s -> S.singleton s

    _      -> S.empty

instance Types a => Types [a] where
  apply = fmap . apply

  free = foldr (S.union . free) S.empty

instance Types Scheme where
  apply s (Scheme as t) =
    Scheme as $ apply (foldr M.delete s as) t

  free (Scheme as t) =
    free t `S.difference` S.fromList as

instance Types TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env

  free (TypeEnv env) = free $ M.elems env

emptySubst :: Subst
emptySubst =
  M.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 =
  M.map (apply s1) s2 `M.union` s1

data Error
  = TypesDoNotUnify Type Type
  | OccursCheckFails Text Type
  | UnboundVariable Id

instance Show Error where
  show = \case
    TypesDoNotUnify t u ->
      "Types do not unify: " <> show t <> " vs. " <> show u

    OccursCheckFails u t ->
      "Occurs check fails: " <> T.unpack u <> " vs. " <> show t

    UnboundVariable name ->
      "Unbound variable: " <> T.unpack name

data IState = IState
  { _env    :: TypeEnv
  , _varId  :: Int
  , _errors :: [Error]
  } deriving Show

makeClassy ''IState

initialIState :: IState
initialIState = IState
  { _varId = 0
  , _env = emptyTypeEnv
  , _errors = []
  }

type Infer = State IState

remove :: TypeEnv -> Id -> TypeEnv
remove (TypeEnv e) name =
  TypeEnv $ M.delete name e

fresh :: Infer Type
fresh = do
  vid <- use varId
  varId += 1
  return $ TVar $ "a" <> T.pack (show vid)

generalize :: TypeEnv -> Type -> Scheme
generalize tyEnv t =
  let
    vars = S.toList (free t `S.difference` free tyEnv)
  in
  Scheme vars t

instantiate :: Scheme -> Infer Type
instantiate (Scheme _ t) = do
  return t

addError :: Error -> Infer ()
addError err = do
  errs <- use errors
  errors .= errs ++ [err]

lookupEnv :: Id -> Infer Type
lookupEnv x = do
  (TypeEnv e) <- use env
  case M.lookup x e of
    Nothing -> do
      addError $ UnboundVariable x
      return TUnit

    Just s ->
      instantiate s

occurs :: Types a => Text -> a -> Bool
occurs a t =
  a `S.member` free t

bind :: Id -> Type -> Infer Subst
bind a t
  | t == TVar a =
    return emptySubst

  | occurs a t = do
    addError $ OccursCheckFails a t
    return emptySubst

  | otherwise =
    return $ M.singleton a t

mgu :: Type -> Type -> Infer Subst
mgu t1 t2 = case (t1, t2) of
  (TVar a, t)               ->
    bind a t

  (t, TVar a)               ->
    bind a t

  (TCon a, TCon b) | a == b ->
    return emptySubst

  _                         -> do
    addError $ TypesDoNotUnify t1 t2
    return emptySubst

unify :: Type -> Type -> Subst -> Infer Subst
unify t1 t2 s = do
  u <- mgu (apply s t1) (apply s t2)
  return $ u `compose` s

runInfer :: IState -> Infer (Expr, Type) -> Either [Error] (Expr, IState)
runInfer st ti =
  let
    ((e, _), st') =
      runState ti st
  in
  case st' ^. errors of
    []   -> Right (e, st')
    errs -> Left errs

run :: [Expr] -> Either [Error] ([Expr], TypeEnv)
run es' =
  let
    aux st errs es = \case
      [] -> Right ([], st)

      [x] -> do
        (e, st') <- left (errs ++) (runInfer st $ tiExpr x)
        if null errs
          then Right (es ++ [e], st')
          else Left errs

      x:xs ->
        case runInfer st $ tiExpr x of
          Right (e, st') ->
            aux st' errs (es ++ [e]) xs

          Left err ->
            aux st (errs ++ err) es xs
  in
  aux initialIState [] [] es'
  |> id +++ (\(es, IState{..}) -> (es, _env))

tiExpr :: Expr -> Infer (Expr, Type)
tiExpr = \case
  e -> do
    (_, e1, t) <- tiExpr' e emptySubst
    return (e1, t)

tiExpr' :: Expr -> Subst -> Infer (Subst, Expr, Type)
tiExpr' e subst = case e of
  EUnit ->
    return (emptySubst, e, TUnit)

  EInt _ ->
    return (emptySubst, e, TInt)

  EString _ ->
    return (emptySubst, e, TString)

  EId name -> do
    t <- lookupEnv name
    return (emptySubst, e, t)

  ELet name expr -> do
    env_ <- use env
    a <- fresh

    let scheme = generalize (apply subst env_) a
    env .= env_ `extend` (name, scheme)

    (s1, e1, t1) <- tiExpr' expr subst

    s <- unify a t1 (s1 `compose` subst)
    let scheme' = generalize (apply s env_) t1
    env .= env_ `extend` (name, scheme')

    return
      ( s
      , ELet name e1
      , TUnit
      )

  ELambda Nothing expr -> do
    a <- fresh
    (s1, e1, t1) <- tiExpr' expr subst
    s <- unify a t1 (s1 `compose` subst)
    return
      ( s
      , ELambda Nothing e1
      , apply s t1
      )

  ELambda (Just arg) expr -> do
    a <- fresh
    b <- fresh

    env_ <- use env
    let scheme = generalize (apply subst env_) a
    env .= env_ `extend` (arg, scheme)

    (s1, e1, t1) <- tiExpr' expr subst
    s <- unify b t1 (s1 `compose` subst)

    env .= env_

    return
      ( subst
      , ELambda (Just arg) e1
      , TArr (apply s a) (apply s b)
      )

  EApp _ _ ->
    undefined
