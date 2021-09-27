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
  deriving Eq

pattern TUnit :: Type
pattern TUnit = TCon "()"

pattern TInt :: Type
pattern TInt = TCon "Int"

pattern TString :: Type
pattern TString = TCon "String"

showType :: Type -> Text
showType = \case
  TVar x -> "~" <> x

  TCon x -> x

instance Show Type where
  show = unpack . showType
