import System.Exit

import PLTests
import QCTests

-- Performs all tests.
runAllTests :: IO Bool
runAllTests = (foldr (&&) True <$>) $ sequence $ [runAllQCTests, runAllPLTests]

-- Performs all tests and uses the exit code to signal
-- whether all tests have passed or not.
main :: IO ()
main = do
  result <- runAllTests
  if result
    then exitSuccess
    else exitFailure
