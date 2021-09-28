module Language.Type
  ( Id
  , Type(..)
  , show
  , pattern TUnit
  , pattern TInt
  , pattern TString
  ) where

import           Data.Text

type Id =
  Text

data Type
  = TVar Text
  | TCon Text
  | TArr Type Type
  deriving Eq

pattern TUnit :: Type
pattern TUnit = TCon "()"

pattern TInt :: Type
pattern TInt = TCon "Int"

pattern TString :: Type
pattern TString = TCon "String"

showType :: Type -> Text
showType = \case
  TVar x   -> "~" <> x

  TCon x   -> x

  TArr l r -> showType l <> " -> " <> showType r

instance Show Type where
  show = unpack . showType
