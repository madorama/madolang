module Language.Syntax where

import           Data.Text

data Expr
  = EInt Integer
  | EString Text
  | EId Text
  | ELet Text Expr
  | ELambda (Maybe Text) Expr
  deriving (Show, Eq)

mkLambda :: [Text] -> Expr -> Expr
mkLambda args expr =
  case args of
    [] ->
      ELambda Nothing expr

    [x] ->
      ELambda (Just x) expr

    x:xs ->
      ELambda (Just x) (mkLambda xs expr)
