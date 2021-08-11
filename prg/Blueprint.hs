{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Console.CmdArgs
import Control.Monad
import Blueprint.Interpreter
import Paths_blueprint (version)
import Data.Version (showVersion)

data BlueprintArguments = BlueprintArguments
  { verbose  :: Bool,
    fileName :: String }
  deriving (Show, Data, Typeable)

config = cmdArgsMode $ BlueprintArguments {
  verbose = False &= explicit
          &= help "Enable verbose execution"
          &= name "verbose"
          &= name "v",
  fileName = def
             &= typ "SourceFile"
             &= argPos 0
  } &= summary ("Blueprint Scheme v" ++ showVersion version)
    &= help    "Execute scheme programs"
    &= program "blueprint"
    &= helpArg [explicit, name "h", name "help"]

main :: IO ()
main = do
  args <- cmdArgsRun config
  program <- readFile $ fileName args
  putStrLn program
  putStr " → "
  print $ parseval program
