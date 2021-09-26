{-# LANGUAGE ApplicativeDo   #-}

module Madlib.Megaparsec.Token where

import           Control.Monad
import qualified Data.HashSet               as HashSet
import qualified Data.Text                  as Text
import qualified Text.Megaparsec            as P
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Madlib.Megaparsec.Language
import           Madlib.Megaparsec.Parser

parens :: Applicative m => (Text.Text -> m close) -> m a -> m a
parens p = P.between (p "(") (p ")")

braces :: Applicative m => (Text.Text -> m close) -> m a -> m a
braces p = P.between (p "{") (p "}")

angles :: Applicative m => (Text.Text -> m close) -> m a -> m a
angles p = P.between (p "<") (p ">")

brackets :: Applicative m => (Text.Text -> m close) -> m a -> m a
brackets p = P.between (p "[") (p "]")

comma :: (Text.Text -> t) -> t
comma p = p ","

colon :: (Text.Text -> t) -> t
colon p = p ":"

semi :: (Text.Text -> t) -> t
semi p = p ";"

dot :: (Text.Text -> t) -> t
dot p = p "."

semiSep :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
semiSep p p2 = P.sepBy p2 $ semi p

semiSep1 :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
semiSep1 p p2 = P.sepBy1 p2 $ semi p

commaSep :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
commaSep p p2 = P.sepBy p2 $ comma p

commaSep1 :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
commaSep1 p p2 = P.sepBy1 p2 $ comma p

ifp :: P.MonadParsec e s m => m a -> m Bool
ifp p = P.option False (True <$ p)

lexeme :: P.MonadParsec e s m => LanguageStyle m -> m a -> m a
lexeme langStyle = L.lexeme $ spaceConsumer langStyle

symbol :: MonadTextParsec e s m => LanguageStyle m -> P.Tokens s -> m (P.Tokens s)
symbol langStyle = L.symbol $ ws langStyle

keyword :: MonadTextParsec e s m => LanguageStyle m -> Text.Text -> m ()
keyword langStyle word = void $ lexeme langStyle (string word <* P.notFollowedBy (identLetter langStyle))

reserved :: MonadTextParsec e s m => LanguageStyle m -> m ()
reserved langStyle =
  void $ P.choice $ map (P.try . keyword langStyle) $ HashSet.toList (reservedNames langStyle)

stringLiteral :: MonadTextParsec e s m => m Text.Text
stringLiteral = char '"' >> Text.pack <$> P.manyTill L.charLiteral (char '"')

ws :: MonadTextParsec e s m => LanguageStyle m -> m ()
ws langStyle =
  let skipLine = L.skipLineComment $ commentLine langStyle
      skipBlockComment
        | nestedComments langStyle = L.skipBlockCommentNested
        | otherwise = L.skipBlockComment
      skipBlock = skipBlockComment (commentStart langStyle) (commentEnd langStyle)
  in
  L.space (P.skipSome space1) skipBlock skipLine

identifier :: MonadTextParsec e s m => LanguageStyle m -> m Text.Text
identifier langStyle =
  P.label "identifier" $
    lexeme langStyle $ do
      let
        ident =
          (:) <$> identStart langStyle <*> P.many (identLetter langStyle)

      if caseSensitive langStyle then do
        P.notFollowedBy $ reserved langStyle
        Text.pack <$> ident
      else do
        P.notFollowedBy $ reserved $ langStyle { reservedNames = HashSet.map Text.toLower (reservedNames langStyle) }
        Text.pack <$> ident
