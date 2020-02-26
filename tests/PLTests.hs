module PLTests (runAllPLTests) where

import Control.Monad (unless, void)
import Data.Either (isRight)
import Data.List (isPrefixOf)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (listDirectory)

import Parser
import Pretty
import SLD
import TestUtils
import Type

data ParseExcept = Ignore | Error String

-- Parses a list of goals from a file.
parseTestFile :: FilePath -> ExceptT ParseExcept IO [Goal]
parseTestFile fp = do
  lns <- liftIO $ (trim <$>) <$> lines <$> readFile fp
  case lns of
    (first:_) | isIgnore first -> throwE Ignore
    _                          -> liftEither $ mapLeft Error
                                             $ mapM parse
                                             $ filter (not . isComment)
                                             $ filter (not . null)
                                             $ lns
  where isComment ('%':_) = True
        isComment _       = False
        isIgnore          = ("% IGNORE" `isPrefixOf`)

-- Runs the Prolog test at the given path. The
-- associated rule file is expected to be located
-- in a sibling directory called 'rules'.
doPrologTest :: FilePath -> ExceptT ([String], ParseExcept) IO ()
doPrologTest fp = do
  testGoals <- ExceptT $ mapLeft (pair [])         <$> (runExceptT $ parseTestFile fp)
  ruleProg  <- ExceptT $ mapLeft (pair [] . Error) <$> (parseFile $ (takeDirectory . takeDirectory) fp </> "rules" </> takeFileName fp)
  let outcomes  = zip testGoals $ not <$> null <$> solve defaultStrategy ruleProg <$> testGoals
      messages  = toMessage <$> outcomes
  unless (foldr (&&) True $ snd <$> outcomes) $ throwE (messages, Error "Some assertions failed")
  where toMessage (g, b) = pretty g ++ " -> " ++ (if b then "Success" else "Failure")

-- Runs a single Prolog test and possibly outputs the failure message.
runPrologTest :: FilePath -> IO Bool
runPrologTest fp = do
  putStrLn $ "=== Prolog test " ++ fp ++ " ==="
  outcome <- runExceptT $ doPrologTest fp
  case outcome of
    Left (msgs, Error e)  -> do void $ mapM putStrLn msgs
                                putStrLn $ "Error: " ++ e
    Left (_, Ignore)      -> putStrLn "Ignored"
    Right _               -> putStrLn "Success"
  putStrLn ""
  return $ isRight $ outcome

-- Runs all Prolog tests.
runAllPLTests :: IO Bool
runAllPLTests = do
  let dir = "examples/tests"
  contents <- ((dir </>) <$>) <$> listDirectory dir -- TODO: Do not depend on PWD
  passes <- mapM runPrologTest contents
  return $ foldr (&&) True passes
