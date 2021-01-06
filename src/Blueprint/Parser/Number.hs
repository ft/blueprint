module Blueprint.Parser.Number (readInteger) where

import Prelude hiding (read)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Blueprint.AbstractSyntaxTree
import Blueprint.Parser.Internal

readInteger :: Parser SchemeExpression
readInteger = ScmInteger <$> lexeme (L.signed (return()) (lexeme L.decimal))
