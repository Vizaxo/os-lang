module Repl where

import Data.Bifunctor
import Data.Text

import Parser
import Language

data ReplError
  = ParseError LexerParserError
  | InterpError InterpError
  | FileError
  deriving Show

rep :: Text -> Either ReplError [Term]
rep s = do
  terms <- first ParseError (parseTerms s)
  first InterpError (evalInterpreter $ traverse eval terms)

evalFile :: FilePath -> IO (Either ReplError [Term])
evalFile file = rep . pack <$> readFile file
