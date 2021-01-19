module Blueprint.Interpreter.Prelude (prelude) where

import qualified Data.Map.Strict as Map
import Blueprint.Value

prelude :: SchemeEnvironment
prelude = [Map.fromList [("define", Special Define),
                         ("lambda", Special Lambda),
                         ("if",     Special Conditional),
                         ("set!",   Special SetBang),
                         ("inc",    Builtin Increment),
                         ("dec",    Builtin Decrement),
                         ("<",      Builtin LessThan),
                         ("*",      Builtin Multiply),
                         ("+",      Builtin Add),
                         ("-",      Builtin Subtract),
                         ("list",   Builtin MakeList),
                         ("car",    Builtin Car),
                         ("cdr",    Builtin Cdr)]]
