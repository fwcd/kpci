{-# LANGUAGE TemplateHaskell #-}
module REPL (runREPL) where

import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Lang
import Parser
import Pretty
import SLD
import System.Exit
import System.Console.Haskeline
import Type

import Paths_kpci

-- Holds state used by the interactive shell, e.g. the loaded program and the SLD resolution strategy.
-- If no strategy is provided, the SLD tree is output directly.
data REPLState = REPLState Lang Prog (Maybe FilePath) (Maybe Strategy)

-- Runs an interactive Prolog shell.
runREPL :: IO ()
runREPL = do
  asciiArtPath <- getDataFileName "resources/kpcilogo.txt"
  asciiArt <- readFile asciiArtPath

  let welcomeMsg = unlines $ (lines asciiArt) ++ ["", "KIEL PROLOG COMPILER INTERACTIVE", "", localize lang "welcome", localize lang "helpAnno"]
  putStrLn welcomeMsg

  repl st

  where lang = EN
        st = REPLState lang (Prog []) Nothing $ Just defaultStrategy

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
                                  putStrLn $ localize lang "unknowncmd"
                                  return st
  where (REPLState lang _ _ _) = st

-- Evaluates a goal, either by printing the SLD tree or the final substitutions,
-- depending on the strategy.
evaluateGoal :: String -> REPLState -> IO REPLState
evaluateGoal input st = do case parse input of
                             Left e -> putStrLn e
                             Right g -> case strat of
                               Just s  -> putStrLn $ unlines $ map pretty $ solve s p g
                               Nothing -> putStrLn $ pretty $ sld defaultStrategy p g
                           return st
  where (REPLState _ p _ strat) = st

-- A command that takes args and a state and returns a new state.
type Command = String -> REPLState -> IO REPLState

-- The default command registry.
commands :: [(Char, String, String, Command)]
commands = [('l', "<file>",  "Loads the specified file",           loadFile)
           ,('r', "",        "Reloads the file",                   reloadFile)
           ,('s', "<strat>", "Sets the specified search strategy", setStrat)
           ,('q', "",        "Exits the interactive environment",  quit)
           ,('t', "<lang>",  "Sets the language",                  setLang)
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
      putStrLn $ localize lang "loaded" ++ show (length rs) ++ localize lang "rule"
      return $ REPLState lang p (Just path) strat
      where (REPLState lang _ _ strat) = st

-- Reloads the current file.
reloadFile :: Command
reloadFile _ st = case fp of
                    Just path -> loadFile path st
                    Nothing   -> do
                      putStrLn $ localize lang "nofile"
                      return st
  where (REPLState lang _ fp _) = st

-- Selects a search strategy.
setStrat :: Command
setStrat args st | null args = do
                   putStrLn $ localize lang "nostrat"
                   return $ REPLState lang p fp Nothing
                 | otherwise = case lookup args $ strategies of
                   Just strat -> do
                     putStrLn $ localize lang "strat" ++ args
                     return $ REPLState lang p fp $ Just strat
                   Nothing    -> do
                     putStrLn $ localize lang "stratnotav" ++ intercalate ", " (map fst $ strategies)
                     return st
  where (REPLState lang p fp _) = st

-- Sets the language.
setLang :: Command
setLang args st = case lookup args languages of
  Just l -> do
    putStrLn $ localize l "welcome"
    return $ REPLState l p fp strat
  Nothing   -> do
    putStrLn $ localize lang "langnotfound"
    return st
  where (REPLState lang p fp strat) = st

-- Exits the environment
quit :: Command
quit _ st = do
  putStrLn $ localize lang "goodbye"
  exitSuccess
  where (REPLState lang _ _ _) = st

-- Outputs the list of commands.
help :: Command
help _ st = do
  putStrLn $ unlines $ localize lang "cmdsav" : map (("  " ++) . describe) commands
  return st
  where describe (cmd, argDesc, desc, _) = (':':cmd:' ':argDesc) ++ " " ++ desc
        (REPLState lang _ _ _) = st

-- Trims whitespace around a string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')
