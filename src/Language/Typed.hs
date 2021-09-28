module Language.Typed where

import           Data.Text

import           Language.Type

data IdOrUnit
  = Unit
  | Id Text Type
  deriving (Show, Eq)

data Expr
  = EUnit
  | EInt Integer
  | EString Text
  | EId Text
  | ELet Text Type Expr
  | ELambda IdOrUnit Type Expr
  | EApp Expr Expr
  deriving (Show, Eq)
