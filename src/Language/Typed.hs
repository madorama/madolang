module Language.Typed where

import           Data.Text

import           Language.Type

data Expr
  = EUnit
  | EInt Integer
  | EString Text
  | EId Text
  | ELet Text Type Expr
  | ELambda (Maybe (Text, Type)) Expr
  | EApp Expr Expr
  deriving (Show, Eq)
