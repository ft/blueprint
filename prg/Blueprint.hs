module Main where

import Control.Monad
import Blueprint.Interpreter

program =
  [ "(define fact (lambda (n)",
    "               (if (< n 2)",
    "                 1",
    "                 (* n (fact (- n 1))))))",
    "(fact 10)" ]

main :: IO ()
main = do
  forM_ program putStrLn
  putStr " → "
  print $ parseval $ concat program
