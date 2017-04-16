{-# LANGUAGE OverloadedStrings #-}

module Xcss.Parser
  ( Statement(..)
  , CssParseError
  , parseCss
  , cssParser
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

type CssParseError = ParseError Char Text.Megaparsec.Dec

data Statement =
  Function
  deriving (Show, Eq)

-- | Parse CSS into AST
-- |
-- parseCss :: Parser Statement
parseCss = parse cssParser

-- | CSS Parser
-- |
cssParser :: Parser [Statement]
cssParser = do
  statements <- between whitespace eof topStatements
  pure $ catMaybes statements

-- | Parse a sequence of statements
-- |
topStatements :: Parser [Maybe Statement]
topStatements = manyTill topStatement eof

-- | Parse a sequence of statements seperated by semicolons.
-- |
topStatement :: Parser (Maybe Statement)
topStatement = blockStatement

-- | Rule block
-- |
-- | div { ... }
-- |
blockStatement :: Parser (Maybe Statement)
blockStatement = do
  selector
  rword "{"
  ruleList
  rword "}"
  return Nothing

-- | Parse a list of rules
-- |
ruleList :: Parser [()]
ruleList = lexeme $ sepEndBy rule (rword ";")

-- | Parse a rule
-- |
rule :: Parser ()
rule = do
  ruleIdentifier
  rword ":"
  ruleValue
  pure ()

-- | Parse an identifier word used in a selector
-- |
ruleIdentifier :: Parser String
ruleIdentifier = (lexeme . try) $ (:) <$> startChar <*> many restChar
  where
    punct = char '-' <|> char '_'
    startChar = letterChar <|> punct
    restChar = alphaNumChar <|> punct

-- | Parse a string
-- |
singleQuoteString :: Parser String
singleQuoteString = do
  char '\''
  content <- manyTill anyChar (char '\'')
  pure content

-- | Parse a string
-- |
doubleQuoteString :: Parser String
doubleQuoteString = do
  char '"'
  content <- manyTill anyChar (char '"')
  pure content

-- | Parse a rule's value
-- |
ruleValue :: Parser String
ruleValue = singleQuoteString <|> doubleQuoteString<|> lexeme (many alphaNumChar)

-- | Parse an identifier word used in a selector
-- |
selector :: Parser [String]
selector = many selectorIdentifier

-- | Parse an identifier word used in a selector
-- |
selectorIdentifier :: Parser String
selectorIdentifier = (lexeme . try) $ (:) <$> startChar <*> many restChar
  where
    spacer = char '-' <|> char '_'
    startChar = letterChar <|> char '#' <|> char '.'
    restChar = alphaNumChar <|> spacer

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
