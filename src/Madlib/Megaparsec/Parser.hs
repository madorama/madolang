module Madlib.Megaparsec.Parser where

import           Data.Text
import           Text.Megaparsec

type MonadTextParsec e s m =
  ( Ord e
  , MonadParsec e s m
  , Token s ~ Char
  , Tokens s ~ Text
  )
