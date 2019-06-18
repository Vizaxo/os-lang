module Parser where

import Types
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
cons (SexpTree xs) = List <$> traverse term xs
cons _ = Nothing

symbol :: TokenTree -> Maybe Term
symbol (Node (Identifier s)) = Just (Symbol (Sym s))
symbol _ = Nothing

parseQuote :: TokenTree -> Maybe Term
parseQuote (SexpTree [Node L.Quote, t]) = do
  t' <- term t
  pure (List [Symbol (Sym "quote"), t'])
parseQuote _ = Nothing

parseQuasiquote :: TokenTree -> Maybe Term
parseQuasiquote (SexpTree [Node L.Quasiquote, t]) = do
  t' <- term t
  pure (List [Symbol (Sym "quasiquote"), t'])
parseQuasiquote _ = Nothing

parseUnquote :: TokenTree -> Maybe Term
parseUnquote (SexpTree [Node L.Unquote, t]) = do
  t' <- term t
  pure (List [Symbol (Sym "unquote"), t'])
parseUnquote _ = Nothing

string :: TokenTree -> Maybe Term
string (Node (L.String s)) = Just (Types.String s)
string _ = Nothing

term :: TokenTree -> Maybe Term
term t = parseQuote t <|> parseQuasiquote t <|> parseUnquote t
  <|> string t <|> symbol t <|> cons t

parseTerms :: Text -> Either LexerParserError [Term]
parseTerms s = do
  lexed <- first ErrLex (P.parse lexer "" s)
  sexp <- first ErrSexp (P.parse (sexpTree <* P.eof) "" lexed)
  maybe (Left ErrParse) Right (traverse term sexp)
