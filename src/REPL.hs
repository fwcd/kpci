module REPL (runREPL) where

import Parser
import Pretty
import SLD
import System.Exit
import System.IO (hFlush, stdout)
import Type

-- Holds state used by the interactive shell, e.g. the loaded program and the SLD resolution strategy.
data REPLState = REPLState Prog (Maybe FilePath) Strategy

-- Runs an interactive Prolog shell.
runREPL :: IO ()
runREPL = repl (Just "Welcome!\nType \":h\" for help.") st
  where st = REPLState (Prog []) Nothing (const [])

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
        where (REPLState p _ _) = st
    return st

-- Evaluates a command prefixed with ':'.
evaluateCommand :: String -> REPLState -> IO REPLState
evaluateCommand cmd st = case cmd of
  -- TODO: Implement :h and others
  'l' : path -> loadFile path st
  "r" -> case p of
      Just path -> loadFile path st
      Nothing -> do
        putStrLn "No file loaded yet!"
        return st
    where (REPLState _ p _) = st
  "q" -> do
    putStrLn "Goodbye!"
    exitSuccess
  _   -> return st

-- Loads a file into the interpreter.
loadFile :: FilePath -> REPLState -> IO REPLState
loadFile path st = do
  f <- parseFile path
  case f of
    Left e -> do
      putStrLn e
      return st
    Right p -> do
      putStrLn "Success!"
      return $ REPLState p (Just path) strat
      where (REPLState _ _ strat) = st
