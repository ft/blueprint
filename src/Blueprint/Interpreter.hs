{-# LANGUAGE TupleSections #-}
module Blueprint.Interpreter (eval, parseval) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map

import Blueprint.AbstractSyntaxTree
import Blueprint.Interpreter.Prelude
import Blueprint.Parser
import Blueprint.Parser.Internal
import Blueprint.Value

fromEnvironment :: SchemeEnvironment -> String -> SchemeValue
fromEnvironment [] k = error $ k ++ " not defined"
fromEnvironment (m:ms) k = case Map.lookup k m of
                             Nothing -> fromEnvironment ms k
                             Just v -> v

applyBuiltin :: SchemeBuiltin -> [SchemeValue] -> SchemeValue
applyBuiltin Multiply  [Atom (ScmInteger a),Atom (ScmInteger b)] = Atom $ ScmInteger $ a * b
applyBuiltin Add       [Atom (ScmInteger a),Atom (ScmInteger b)] = Atom $ ScmInteger $ a + b
applyBuiltin Subtract  [Atom (ScmInteger a),Atom (ScmInteger b)] = Atom $ ScmInteger $ a - b
applyBuiltin LessThan  [Atom (ScmInteger a),Atom (ScmInteger b)] = Atom $ if a < b
                                                                          then ScmBool True
                                                                          else ScmBool False
applyBuiltin Increment [Atom (ScmInteger a)] = Atom $ ScmInteger $ a + 1
applyBuiltin Decrement [Atom (ScmInteger a)] = Atom $ ScmInteger $ a - 1
applyBuiltin MakeList rands = List rands
applyBuiltin Car [List (x:_)] = x
applyBuiltin Cdr [List (_:xs)] = List xs
applyBuiltin op rands = error $ show op ++ ": Invalid arguments: " ++ show rands

extendClosureEnv value@(Closure (formals, body, e:es)) sym = Closure (formals, body, Map.insert sym value e : es)
extendClosureEnv value _ = value

define :: String -> SchemeExpression -> State SchemeEnvironment SchemeValue
define sym expr = do
  value <- evalWith expr
  state $ \env -> (Undefined, Map.insert sym (extendClosureEnv value sym) (head env) : tail env)

makeClosure :: [SchemeExpression] -> State SchemeEnvironment SchemeValue
makeClosure ((ScmList args):exprs) = state $ \env -> (Closure (args, Sequence exprs, env), env)

trueish (Atom (ScmBool False)) = False
trueish _ = True

applySpecial :: SchemeSpecialForm -> [SchemeExpression] -> State SchemeEnvironment SchemeValue
applySpecial Define [ScmSymbol sym, expr] = define sym expr
-- TODO: set! should fail unless sym is defined in environment.
applySpecial SetBang [ScmSymbol sym, expr] = define sym expr
applySpecial Lambda exprs = makeClosure exprs
applySpecial Conditional [condition,consequence,alternative] = do
  decision <- evalWith condition
  evalWith $ if trueish decision
             then consequence
             else alternative

insertFormal (ScmSymbol k, v) m = Map.insert k v m
insertFormal (k, _) _ = error $ "Not a proper formal parameter: " ++ show k

bindFormals :: [SchemeExpression] -> [SchemeValue] -> SchemeLookup
bindFormals f a = foldr insertFormal Map.empty $ zip f a

runSequence (Sequence body) = foldM (\_ expr -> evalWith expr) Undefined body
runSequence b = error $ "runSequence, not a sequence: " ++ show b

applyClosure :: SchemeClosure -> [SchemeValue] -> State SchemeEnvironment SchemeValue
applyClosure (formals, body, clenv) args = do
  currentEnvironment <- get
  env <- put $ bindFormals formals args : clenv ++ currentEnvironment
  runSequence body

apply :: SchemeValue -> [SchemeValue] -> State SchemeEnvironment SchemeValue
apply (Builtin op) rands = state (applyBuiltin op rands,)
apply (Closure op) rands = applyClosure op rands
apply op rands = state (Atom $ ScmSymbol $ "Catchall: " ++ show op ++ " " ++ show rands,)

evalWith :: SchemeExpression -> State SchemeEnvironment SchemeValue
evalWith (ScmSymbol sym) = state $ \env -> (fromEnvironment env sym, env)
evalWith (ScmList ((ScmSymbol "begin"):seq)) = foldM (\_ expr -> evalWith expr) Undefined seq
evalWith (ScmList (op:rands)) = do
  operator <- evalWith op
  case operator of
    Special form -> applySpecial form rands
    _            -> do operands <- mapM evalWith rands
                       apply operator operands
evalWith expr = state (Atom expr,)

eval :: SchemeExpression -> SchemeValue
eval expr = evalState (evalWith expr) prelude

parseval :: String -> SchemeValue
parseval str = case readSchemeProgram str of
  Left err -> error $ show err
  Right expr -> eval expr
