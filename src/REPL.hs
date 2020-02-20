module REPL (runREPL) where

import Parser
import Pretty
import SLD
import System.Exit
import System.IO (hFlush, stdout)
import Type

data REPLState = REPLState Prog Strategy

-- Runs an interactive Prolog shell.
runREPL :: IO ()
runREPL = repl (Just "Welcome!\nType \":h\" for help.") st
  where st = REPLState (Prog []) (const [])

-- Runs the REPL loop.
repl :: Maybe String -> REPLState -> IO ()
repl msg st = do
  case msg of
    Just m -> putStrLn m
    Nothing -> return ()

  putStr "?- "
  hFlush stdout

  line <- getLine
  st' <- evaluate line st

  repl Nothing st'

-- Evaluates an input.
evaluate :: String -> REPLState -> IO REPLState
evaluate input st = case input of
  ""        -> return st
  ':' : cmd -> evaluateCommand cmd st
  _         -> do
    -- TODO: Implement a proper, interactive solution output
    case parse input of
      Left e -> putStrLn e
      Right g -> putStrLn $ pretty $ sld p g
        where (REPLState p _) = st
    return st

-- Evaluates a command prefixed with ':'.
evaluateCommand :: String -> REPLState -> IO REPLState
evaluateCommand cmd st = case cmd of
  -- TODO: Implement :h and others
  'l' : path -> do
    f <- parseFile path
    case f of
      Left e -> do
        putStrLn e
        return st
      Right p -> return $ REPLState p strat
        where (REPLState _ strat) = st
  "q" -> do
    putStrLn "Goodbye!"
    exitSuccess
  _   -> return st
