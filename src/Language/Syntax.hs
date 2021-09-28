module Language.Syntax where

import           Data.Text

data Expr
  = EInt Integer
  | EString Text
  | EId Text
  | ELet Text Expr
  deriving (Show, Eq)
