module Blueprint.Value (SchemeValue(..),
                        SchemeSpecialForm(..),
                        SchemeBuiltin(..),
                        SchemeClosure(..),
                        SchemeEnvironment(..),
                        SchemeLookup(..))
where

import qualified Data.Map.Strict as Map
import Blueprint.AbstractSyntaxTree

type SchemeLookup = Map.Map String SchemeValue
type SchemeEnvironment = [SchemeLookup]
type SchemeClosure = ([SchemeExpression], SchemeValue, SchemeEnvironment)

data SchemeSpecialForm = Define
                       | Lambda
                       | Conditional
                       | SetBang
  deriving Show

data SchemeBuiltin = Increment
                   | Decrement
                   | LessThan
                   | Multiply
                   | Add
                   | Subtract
                   | MakeList
                   | Car
                   | Cdr
  deriving Show

data SchemeValue = Atom SchemeExpression
                 | Sequence [SchemeExpression]
                 | List [SchemeValue]
                 | Builtin SchemeBuiltin
                 | Special SchemeSpecialForm
                 | Closure SchemeClosure
                 | Undefined
  deriving Show
