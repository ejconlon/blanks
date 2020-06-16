module Test.Blanks.Parsing where

import Control.Applicative (Alternative (..))
import Control.Exception (throwIO)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

type Parser = MP.Parsec Void String

-- The parser is not the SUT here, so if it fails,
-- so does the test. We ensure valid input.
runParserIO :: Parser a -> String -> IO a
runParserIO p s =
  case MP.runParser p "<test>" s of
    Left e -> throwIO e
    Right a -> pure a

data SourceSpan = SourceSpan
  { _ssName :: !FilePath
  , _ssStartLine :: !MP.Pos
  , _ssStartColumn :: !MP.Pos
  , _ssEndLine :: !MP.Pos
  , _ssEndColumn :: !MP.Pos
  } deriving (Eq, Show, Ord)

mkSourceSpan :: MP.SourcePos -> MP.SourcePos -> SourceSpan
mkSourceSpan (MP.SourcePos n sl sc) (MP.SourcePos _ el ec) = SourceSpan n sl sc el ec

around :: (SourceSpan -> a -> b) -> Parser a -> Parser b
around f pa = (\s a e -> f (mkSourceSpan s e) a) <$> MP.getSourcePos <*> pa <*> MP.getSourcePos

around2 :: (SourceSpan -> a -> b -> c) -> Parser (a, b) -> Parser c
around2 f pab = (\s (a, b) e -> f (mkSourceSpan s e) a b) <$> MP.getSourcePos <*> pab <*> MP.getSourcePos

around3 :: (SourceSpan -> a -> b -> c -> d) -> Parser (a, b, c) -> Parser d
around3 f pabc = (\s (a, b, c) e -> f (mkSourceSpan s e) a b c) <$> MP.getSourcePos <*> pabc <*> MP.getSourcePos

double :: Parser a -> Parser (a, a)
double p = (,) <$> p <*> p

triple :: Parser a -> Parser (a, a, a)
triple p = (,,) <$> p <*> p <*> p

spaceConsumer :: Parser ()
spaceConsumer = MPCL.space MPC.space1 lineCmnt blockCmnt
  where
    lineCmnt = MPCL.skipLineComment ";"
    blockCmnt = MPCL.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = MPCL.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = MPCL.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && c /= ' ' && c /= '\t' && c /= '\n'

identifier :: Parser String
identifier = lexeme (MP.takeWhile1P Nothing nonDelimPred)

-- Take the first successful result, backtracking on failure.
branch :: [Parser a] -> Parser a
branch xs =
  case xs of
    [] -> empty
    x:xs' -> MP.try x <|> branch xs'

signed :: Parser Int
signed = MPCL.signed spaceConsumer (lexeme MPCL.decimal)
