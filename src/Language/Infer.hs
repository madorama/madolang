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

    TArr t1 t2 -> TArr (apply s t1) (apply s t2)

    t          -> t

  free = \case
    TVar s     -> S.singleton s

    TArr t1 t2 -> free t1 `S.union` free t2

    _          -> S.empty

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
  , _subst  :: Subst
  } deriving Show

makeClassy ''IState

initialIState :: IState
initialIState = IState
  { _varId = 0
  , _env = emptyTypeEnv
  , _errors = []
  , _subst = emptySubst
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
instantiate (Scheme as t) = do
  ts <- mapM (const fresh) as
  return (inst ts t)

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts = \case
    TArr l r -> TArr (inst ts l) (inst ts r)

    t        -> t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

addError :: Error -> Infer ()
addError err = do
  errs <- use errors
  errors .= errs ++ [err]

addEnv :: (Id, Type) -> Infer ()
addEnv (name, t) = do
  env_ <- use env
  env .= env_ `extend` (name, generalize env_ t)

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
  (TArr l1 r1, TArr l2 r2)  -> do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s2 `compose` s1

  (TVar a, t)               ->
    bind a t

  (t, TVar a)               ->
    bind a t

  (TCon a, TCon b) | a == b ->
    return emptySubst

  _                         -> do
    addError $ TypesDoNotUnify t1 t2
    return emptySubst

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  s <- use subst
  u <- mgu (apply s t1) (apply s t2)
  subst .= u `compose` s

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
    (e1, t1) <- tiExpr' e
    return (e1, t1)

tiExpr' :: Expr -> Infer (Expr, Type)
tiExpr' e = case e of
  EUnit ->
    return (e, TUnit)

  EInt _ ->
    return (e, TInt)

  EString _ ->
    return (e, TString)

  EId name -> do
    t <- lookupEnv name
    return (e, t)

  ELet name expr -> do
    (e1, t1) <- tiExpr' expr
    addEnv (name, t1)
    return
      ( ELet name e1
      , TUnit
      )

  ELambda Nothing expr -> do
    (e1, t1) <- tiExpr' expr
    return
      ( ELambda Nothing e1
      , TArr TUnit t1
      )

  ELambda (Just arg) expr -> do
    env_ <- use env

    a <- fresh
    addEnv (arg, a)
    (e1, t1) <- tiExpr' expr

    s <- use subst
    env .= env_
    return
      ( ELambda (Just arg) e1
      , TArr (apply s a) t1
      )

  EApp l r -> do
    (e1, t1) <- tiExpr' l
    (e2, t2) <- tiExpr' r
    a <- fresh
    unify t1 (TArr t2 a)
    s <- use subst
    return
      ( EApp e1 e2
      , apply s a
      )
