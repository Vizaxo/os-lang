module Main where

import System.Exit

import Language
import Types

data TestResult
  = Ignored
  | Passed
  | Failed String
  deriving (Eq, Show)

main :: IO ()
main = do
  evalInterpreter (evalFile "test-lisp/tests.lisp") >>= \case
    Left err -> do putStr "Test error: "
                   print err
                   exitFailure
    Right outputs -> do
      let results = filter (/= Ignored) (map test outputs)
      mapM print results
      if (length (filter (/= Passed) results) /= 0)
        then exitFailure
        else exitSuccess

test :: Term -> TestResult
test (Symbol (Sym "passed")) = Passed
test (Symbol (Sym reason)) = Failed reason
test _ = Ignored
