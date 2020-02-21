module REPL (runREPL) where

import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Parser
import Pretty
import SLD
import System.Exit
import System.Console.Haskeline
import Type

-- Holds state used by the interactive shell, e.g. the loaded program and the SLD resolution strategy.
-- If no strategy is provided, the SLD tree is output directly.
data REPLState = REPLState Prog (Maybe FilePath) (Maybe Strategy)

-- Runs an interactive Prolog shell.
runREPL :: IO ()
runREPL = putStrLn "Welcome!\nType \":h\" for help." >> repl st
  where st = REPLState (Prog []) Nothing $ Just defaultStrategy

{-
-- Runs the REPL loop.
repl :: REPLState -> IO ()
repl st = do
  putStr "?- "
  hFlush stdout

  line <- getLine
  st' <- evaluate line st

  repl st'
-}

-- Runs the REPL loop.
repl :: REPLState -> IO ()
repl = runInputT defaultSettings . repl'
    where repl' st = do
                      line <- getInputLine "?- "
                      case line of
                        Just l -> do
                          st' <- liftIO $ evaluate l st
                          repl' st'
                        Nothing -> repl' st

-- Evaluates an input.
evaluate :: String -> REPLState -> IO REPLState
evaluate input st = case input of
  ""           -> return st
  ':':cmd:args -> evaluateCommand cmd (trim args) st
  _            -> evaluateGoal input st

-- Evaluates a command.
evaluateCommand :: Char -> String -> REPLState -> IO REPLState
evaluateCommand cmd args st = case lookupCommand cmd commands of
                                Just f  -> f args st
                                Nothing -> do
                                  putStrLn "No such command found, type \":h\" for help."
                                  return st

-- Evaluates a goal, either by printing the SLD tree or the final substitutions,
-- depending on the strategy.
evaluateGoal :: String -> REPLState -> IO REPLState
evaluateGoal input st = do case parse input of
                             Left e -> putStrLn e
                             Right g -> case strat of
                               Just s  -> putStrLn $ unlines $ map pretty $ solve s p g
                               Nothing -> putStrLn $ pretty $ sld p g
                           return st
  where (REPLState p _ strat) = st

-- A command that takes args and a state and returns a new state.
type Command = String -> REPLState -> IO REPLState

-- The default command registry.
commands :: [(Char, String, String, Command)]
commands = [('l', "<file>",  "Loads the specified file",           loadFile)
           ,('r', "",        "Reloads the file",                   reloadFile)
           ,('s', "<strat>", "Sets the specified search strategy", setStrat)
           ,('q', "",        "Exits the interactive environment",  quit)
           ,('h', "",        "Helps",                              help)
           ]

-- Finds a command in a list of commands.
lookupCommand :: Char -> [(Char, String, String, Command)] -> Maybe Command
lookupCommand _ []                                   = Nothing
lookupCommand cmd ((cmd', _, _, f):cs) | cmd == cmd' = Just f
                                       | otherwise   = lookupCommand cmd cs

-- Loads a file into the interpreter.
loadFile :: Command
loadFile path st = do
  f <- parseFile path
  case f of
    Left e -> do
      putStrLn e
      return st
    Right p@(Prog rs) -> do
      putStrLn $ "Successfully loaded " ++ show (length rs) ++ " rule(s)"
      return $ REPLState p (Just path) strat
      where (REPLState _ _ strat) = st

-- Reloads the current file.
reloadFile :: Command
reloadFile _ st = case fp of
                    Just path -> loadFile path st
                    Nothing   -> do
                      putStrLn "No file loaded yet!"
                      return st
  where (REPLState _ fp _) = st

-- Selects a search strategy.
setStrat :: Command
setStrat args st | null args = do
                   putStrLn "Successfully deselected strategy, subsequent queries will output the entire SLD tree"
                   return $ REPLState p fp Nothing
                 | otherwise = case lookup args $ strategies of
                   Just strat -> do
                     putStrLn $ "Successfully selected strategy '" ++ args ++ "'"
                     return $ REPLState p fp $ Just strat
                   Nothing    -> do
                     putStrLn $ "No such strategy available! Try one of these: " ++ intercalate ", " (map fst $ strategies)
                     return st
  where (REPLState p fp _) = st

-- Exits the environment
quit :: Command
quit _ _ = do
  putStrLn "Goodbye!"
  exitSuccess

-- Outputs the list of commands.
help :: Command
help _ st = do
  putStrLn $ unlines $ "Commands available from the prompt" : map (("  " ++) . describe) commands
  return st
  where describe (cmd, argDesc, desc, _) = (':':cmd:' ':argDesc) ++ " " ++ desc

-- Trims whitespace around a string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')
