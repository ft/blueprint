module Blueprint.Parser.Symbol (read) where

import Prelude hiding (read)

import Text.Megaparsec

import Blueprint.AbstractSyntaxTree
import Blueprint.Parser.Internal

import qualified Blueprint.Parser.Predicates as Pred

read :: Parser SchemeExpression
read = ScmSymbol <$> lexeme sym
  where sym = some $ satisfy Pred.isLetter
