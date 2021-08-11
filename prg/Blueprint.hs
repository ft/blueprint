module Main where

import Control.Monad
import Blueprint.Interpreter

program =
  unlines [ "(define fact (lambda (n)",
            "               (if (< n 2)",
            "                 1",
            "                 (* n (fact (- n 1))))))",
            "(fact 10)" ]

main :: IO ()
main = do
  putStrLn program
  putStr " → "
  print $ parseval program
