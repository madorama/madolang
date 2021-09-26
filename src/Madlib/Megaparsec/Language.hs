module Madlib.Megaparsec.Language where

import qualified Data.HashSet             as HashSet
import           Data.Text
import           Text.Megaparsec          ((<|>))
import           Text.Megaparsec.Char

import           Madlib.Megaparsec.Parser

data LanguageStyle m = LanguageStyle
  { spaceConsumer  :: m ()
  , identStart     :: m Char
  , identLetter    :: m Char
  , commentLine    :: Text
  , commentStart   :: Text
  , commentEnd     :: Text
  , reservedNames  :: HashSet.HashSet Text
  , nestedComments :: Bool
  , caseSensitive  :: Bool
  }

emptyStyle :: MonadTextParsec e s m => LanguageStyle m
emptyStyle = LanguageStyle
  { spaceConsumer = space
  , identStart = letterChar <|> char '_'
  , identLetter = alphaNumChar <|> char '_'
  , commentLine = ""
  , commentStart = ""
  , commentEnd = ""
  , reservedNames = HashSet.empty
  , nestedComments = False
  , caseSensitive = True
  }

