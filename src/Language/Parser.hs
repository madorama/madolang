module Language.Parser
  ( run
  ) where

import           Data.Text
import           Data.Void
import           Text.Megaparsec hiding (Label, unexpected)

import           Language.Syntax
import           Language.Token

type Source =
  Text

run :: FilePath -> Source -> Either (ParseErrorBundle Text Void) [Expr]
run =
  runParser $ lexeme parseProgram <* eof

parseProgram :: Parser [Expr]
parseProgram =
  many parseExpr

parseExpr :: Parser Expr
parseExpr =
  choice
    [ parseLiteral
    ]

parseLiteral :: Parser Expr
parseLiteral =
  choice
    [ EInt <$> integer
    , EString <$> stringLiteral
    ]
    <?> "literal"
