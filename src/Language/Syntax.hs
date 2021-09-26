module Language.Syntax where

import Data.Text

data Expr
  = EInt Integer
  | EString Text
  deriving (Show, Eq)
