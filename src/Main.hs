module Main where

import GHC.IO.Encoding
import REPL

main :: IO ()
main = do
  setLocaleEncoding utf8
  runREPL
