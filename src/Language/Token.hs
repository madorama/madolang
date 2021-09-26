module Language.Token where

import qualified Data.HashSet               as HS
import           Data.Text
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Madlib.Megaparsec.Language
import           Madlib.Megaparsec.Parser
import qualified Madlib.Megaparsec.Token     as T

type Parser =
  Parsec Void Text

keywords :: [Text]
keywords =
  []

style :: MonadTextParsec e s m => LanguageStyle m
style = emptyStyle
  { commentLine = "//"
  , commentStart = "/*"
  , commentEnd = "*/"
  , nestedComments = True
  , reservedNames = HS.fromList keywords
  }

ws :: Parser ()
ws = T.ws style

lexeme :: Parser a -> Parser a
lexeme p = ws *> T.lexeme style p <* ws

symbol :: Text -> Parser Text
symbol s = try $ ws *> T.symbol style s <* ws

identifier :: Parser Text
identifier = lexeme $ T.identifier style

parens :: Parser a -> Parser a
parens = T.parens symbol

braces :: Parser a -> Parser a
braces = T.braces symbol

angles :: Parser a -> Parser a
angles = T.angles symbol

brackets :: Parser a -> Parser a
brackets = T.brackets symbol

comma :: Parser Text
comma = T.comma symbol

colon :: Parser Text
colon = T.colon symbol

semi :: Parser Text
semi = T.semi symbol

dot :: Parser Text
dot = T.dot symbol

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep symbol

semiSep1 :: Parser a -> Parser [a]
semiSep1 = T.semiSep1 symbol

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep symbol

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 symbol

string :: MonadParsec e s m => Tokens s -> m (Tokens s)
string = C.string

stringLiteral :: Parser Text
stringLiteral = T.stringLiteral

charLiteral :: Parser Char
charLiteral = L.charLiteral

ifp :: MonadParsec e s m => m a -> m Bool
ifp = T.ifp

operator :: Text -> Parser ()
operator o =
  try $ lexeme $ string o >> notFollowedBy (oneOf ("=+_-*&^%$#@![]{}':\\;\".,<\'>" :: String))

decimal :: RealFloat a => Parser a
decimal =
  L.signed ws L.float

integer :: Num a => Parser a
integer =
  choice
    [ try $ C.string "0x" *> try (lexeme L.hexadecimal)
    , try $ C.string "0b" *> try (lexeme L.binary)
    , L.signed ws L.decimal
    ]

bool :: Parser Bool
bool =
  choice
    [ True <$ string "true"
    , False <$ string "false"
    ]
