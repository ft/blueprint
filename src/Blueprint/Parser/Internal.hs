{-# LANGUAGE TypeFamilies #-}

module Blueprint.Parser.Internal (
  Parser,
  ParserError,
  lexeme,
  spaceConsumer) where

import Data.Void
import Text.Megaparsec

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParserError = (ParseErrorBundle String Void)

lineComment :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m ()
lineComment = L.skipLineComment ";"

blockComment :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m ()
blockComment = L.skipBlockComment "#|" "|#"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
