module Language.Parser
  ( run
  ) where

import           Control.Monad.Combinators.Expr
import           Data.Text
import           Data.Void
import           Text.Megaparsec                hiding (Label, unexpected)

import           Madlib.Operator

import           Language.Syntax
import           Language.Token
import           Language.Type

type Source =
  Text

run :: FilePath -> Source -> Either (ParseErrorBundle Text Void) [Expr]
run =
  runParser $ lexeme parseProgram <* eof

parseProgram :: Parser [Expr]
parseProgram =
  many parseExpr

parseType :: Parser Type
parseType =
  let
    p =
      choice
        [ TUnit <$ symbol "()"
        , TInt <$ symbol "int"
        , TString <$ symbol "string"
        , TVar <$> identifier
        ]
  in
  [ [ InfixR (TArr <$ symbol "->")]
  ]
    |> makeExprParser p

parseExpr :: Parser Expr
parseExpr =
  parseSimpleExpr Nothing

parseSimpleExpr :: Maybe Expr -> Parser Expr
parseSimpleExpr = \case
  Just e ->
    choice
      [ parseApp e
          >>= parseSimpleExpr . Just
      , return e
      ]

  Nothing -> do
    e <-
      choice
        [ parseLet
        , parseLambda
        , parseLiteral
        ]
    parseSimpleExpr $ Just e

parseLet :: Parser Expr
parseLet =
  ELet
  <$> try (symbol "let" *> identifier)
  <*> try (optional $ operator ":" *> parseType)
  <*> try (operator "=" *> parseExpr)

parseLambda :: Parser Expr
parseLambda = do
  args <- try (parens $ commaSep identifier)
  operator "->"
  body <- try parseExpr
  return $ mkLambda args body

parseApp :: Expr -> Parser Expr
parseApp e =
  choice
    [ try $ parens (EApp e <$> parseExpr)
    , EApp e <$> (EUnit <$ symbol "()")
    ]

parseLiteral :: Parser Expr
parseLiteral =
  choice
    [ EInt <$> integer
    , EString <$> stringLiteral
    , EId <$> identifier
    ]
    <?> "literal"
