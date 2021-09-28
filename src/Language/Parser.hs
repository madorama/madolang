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
    [ parseLet
    , parseLambda
    , parseLiteral
    ]

parseLet :: Parser Expr
parseLet =
  ELet
  <$> try (symbol "let" *> identifier)
  <*> try (operator "=" *> parseExpr)

parseLambda :: Parser Expr
parseLambda = do
  args <- try (parens $ commaSep identifier)
  operator "->"
  body <- try parseExpr
  return $ mkLambda args body

parseLiteral :: Parser Expr
parseLiteral =
  choice
    [ EInt <$> integer
    , EString <$> stringLiteral
    , EId <$> identifier
    ]
    <?> "literal"
