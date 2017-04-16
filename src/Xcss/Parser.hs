{-# LANGUAGE OverloadedStrings #-}

module Xcss.Parser
  ( Statement(..)
  , CssParseError
  , parseCss
  , cssParser
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String -- input stream is of type ‘String’

type CssParseError = ParseError Char Text.Megaparsec.Dec

data Statement
  = Statements [Statement]
  | Block String
  deriving (Show, Eq)

-- | Parse CSS into AST
-- |
-- parseCss :: Parser Statement
parseCss = parse cssParser

-- | CSS Parser
-- |
cssParser :: Parser Statement
cssParser = between whitespace eof topStatements

-- | Parse a sequence of statements
-- |
topStatements :: Parser Statement
topStatements = Statements <$> manyTill topStatement eof

-- | Parse a sequence of statements seperated by semicolons.
-- |
topStatement :: Parser Statement
topStatement = blockStatement

-- | Rule block
-- |
-- | div { ... }
-- |
blockStatement :: Parser Statement
blockStatement = do
  selector <- identifier
  rword "{"
  rword "}"
  return $ Block selector

-- | Parse an identifier word
-- |
identifier :: Parser String
identifier = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar

-- | Parser a reserved word/symbol
-- |
rword :: String -> Parser ()
rword word = string word *> notFollowedBy alphaNumChar *> whitespace

-- | Consumes whitespace, returning nothing.
-- |
whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = empty
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Whitespace will be consumed after every lexeme automatically,
-- | but not before it.
-- |
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Parse a given word, followed by whitespace.
-- |
symbol :: String -> Parser String
symbol = L.symbol whitespace

-- | Parse something between parenthesis.
-- |
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse an integer.
-- |
integer :: Parser Integer
integer = lexeme L.integer

-- | Parse a semicolon.
-- |
semi :: Parser String
semi = symbol ";"
