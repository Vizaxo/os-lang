-- | Parse the lexed output into a sexp-tree, ready to be converted into terms.
module Sexp where

import Lexer

import Data.Functor.Foldable.TH
import Text.Parsec

data SexpTree t = SexpTree [SexpTree t] | Node t
makeBaseFunctor ''SexpTree

type TokenTree = SexpTree Token

sexp :: TokParser [TokenTree] -> TokParser TokenTree
sexp = (SexpTree <$>) . between (expect OpenParen) (expect CloseParen)

quote :: TokParser TokenTree
quote = expect Quote *> (mkQuote <$> singleSexpTree) where
  mkQuote t = SexpTree [Node Quote, t]

quasiquote :: TokParser TokenTree
quasiquote = expect Quasiquote *> (mkQuasiquote <$> singleSexpTree) where
  mkQuasiquote t = SexpTree [Node Quasiquote, t]

unquote :: TokParser TokenTree
unquote = expect Unquote *> (mkUnquote <$> singleSexpTree) where
  mkUnquote t = SexpTree [Node Unquote, t]

node :: TokParser TokenTree
node = Node <$> notParen

notParen :: TokParser Token
notParen = tokenPrim show (\p _ _ -> p)
  (\t -> if (notElem t [OpenParen, CloseParen]) then Just t else Nothing)

singleSexpTree :: TokParser TokenTree
singleSexpTree = quote <|> quasiquote <|> unquote <|> sexp sexpTree <|> node

sexpTree :: TokParser [TokenTree]
sexpTree = many singleSexpTree
