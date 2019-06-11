module Parser where

import Language
import Lexer as L
import Sexp

import Data.Bifunctor
import Data.Text hiding (cons)
import Control.Applicative
import qualified Text.Parsec as P

data LexerParserError
  = ErrLex P.ParseError
  | ErrSexp P.ParseError
  | ErrParse
  deriving Show

data ParseError
  = CantParse TokenTree

cons :: TokenTree -> Maybe Term
cons (SexpTree xs) = Cons <$> traverse term xs
cons _ = Nothing

symbol :: TokenTree -> Maybe Term
symbol (Node (Identifier s)) = Just (Symbol (Sym s))
symbol _ = Nothing

term :: TokenTree -> Maybe Term
term t = symbol t <|> cons t

parseTerms :: Text -> Either LexerParserError [Term]
parseTerms s = do
  lexed <- first ErrLex (P.parse lexer "" s)
  sexp <- first ErrSexp (P.parse sexpTree "" lexed)
  maybe (Left ErrParse) Right (traverse term sexp)
