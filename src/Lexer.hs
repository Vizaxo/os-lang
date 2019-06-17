module Lexer where

import Text.Parsec
import Text.Parsec.Text

data Token
  = Identifier String
  | OpenParen
  | CloseParen
  | Quote
  | Quasiquote
  | Unquote
  deriving (Eq, Show)

type TokParser = Parsec [Token] ()

identifier :: Parser Token
identifier = Identifier <$> (many1 (alphaNum <|> oneOf "-_"))

specialChars :: [(String, Token)]
specialChars =
  [ ("(", OpenParen)
  , (")", CloseParen)
  , ("'", Quote)
  , ("`", Quasiquote)
  , (",", Unquote)
  ]

specialChar :: Parser Token
specialChar = choice (try . parseSpecialChar <$> specialChars)
  where
    parseSpecialChar :: (String, t) -> Parser t
    parseSpecialChar (s, t) = t <$ string s

tok :: Parser Token
tok = identifier <|> specialChar

expect :: Token -> TokParser ()
expect t = tokenPrim show (\p _ _ -> p) (\t' -> if (t' == t) then Just () else Nothing)

lexer :: Parser [Token]
lexer = spaces *> endBy tok spaces <* eof
