module PLTests (runAllPLTests) where

import Control.Monad (void, unless)
import Data.Either (isRight)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (listDirectory)

import Parser
import Pretty
import SLD
import TestUtils
import Type

-- Parses a list of goals from a file.
parseTestFile :: FilePath -> ExceptT String IO [Goal]
parseTestFile fp = do
  raw <- liftIO $ readFile fp
  liftEither $ mapM parse $ lines raw

-- Runs the Prolog test at the given path. The
-- associated rule file is expected to be located
-- in a sibling directory called 'rules'.
doPrologTest :: FilePath -> ExceptT String IO ()
doPrologTest fp = do
  testGoals <- parseTestFile fp
  ruleProg  <- liftEither =<< (liftIO $ parseFile $ (takeDirectory . takeDirectory) fp </> "rules" </> takeFileName fp)
  let outcomes  = zip testGoals $ not <$> null <$> solve defaultStrategy ruleProg <$> testGoals
      messages  = toMessage <$> outcomes
  liftIO $ void $ mapM putStrLn messages
  unless (foldr (&&) True $ snd <$> outcomes) $ throwE "Some assertions failed"
  where toMessage (g, b) = pretty g ++ " -> " ++ (if b then "Success" else "Failure")

-- Runs a single Prolog test and possibly outputs the failure message.
runPrologTest :: FilePath -> IO Bool
runPrologTest fp = do
  putStrLn $ "=== Prolog test " ++ fp ++ " ==="
  outcome <- runExceptT $ doPrologTest fp
  putStrLn $ case outcome of
    Left e  -> "Error: " ++ e
    Right _ -> "Success"
  putStrLn ""
  return $ isRight $ outcome

-- Runs all Prolog tests.
runAllPLTests :: IO Bool
runAllPLTests = do
  let dir = "examples/tests"
  contents <- ((dir </>) <$>) <$> listDirectory dir -- TODO: Do not depend on PWD
  passes <- mapM runPrologTest contents
  return $ foldr (&&) True passes
