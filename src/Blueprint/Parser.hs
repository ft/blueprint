{-# LANGUAGE OverloadedStrings #-}

module Blueprint.Parser (parseScheme,
                         readScheme,
                         schemeExpression) where

import Prelude hiding (readList)

import Text.Megaparsec
import Text.Megaparsec.Char

import Blueprint.AbstractSyntaxTree
import Blueprint.Parser.Internal

import qualified Blueprint.Parser.Boolean as Boolean
import qualified Blueprint.Parser.Number as Number
import qualified Blueprint.Parser.Symbol as Symbol

readScheme :: String -> Either ParserError SchemeExpression
readScheme = parse parseScheme ""

parseScheme :: Parser SchemeExpression
parseScheme = spaceConsumer *> schemeExpression <* eof

schemeExpression :: Parser SchemeExpression
schemeExpression = try Boolean.read
               <|> try Number.readInteger
               <|> try Symbol.read
               <|> try readList

readList :: Parser SchemeExpression
readList = ScmList <$> (open *> some schemeExpression <* close)
  where open  = lexeme $ char '('
        close = lexeme $ char ')'
