module Blueprint.Parser.Boolean (read) where

import Prelude hiding (read)

import Data.Functor

import Text.Megaparsec
import Text.Megaparsec.Char

import Blueprint.AbstractSyntaxTree
import Blueprint.Parser.Internal

read :: Parser SchemeExpression
read = true <|> false
  where symbolBool l s = string l <|> string s
        symbolTrue  = symbolBool "#true"  "#t"
        symbolFalse = symbolBool "#false" "#f"
        pb f v = ScmBool <$> lexeme (f $> v)
        true  = pb symbolTrue  True
        false = pb symbolFalse False
