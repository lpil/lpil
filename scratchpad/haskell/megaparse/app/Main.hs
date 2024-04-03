{-# LANGUAGE OverloadedStrings #-}

-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
module Main where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String -- input stream is of type ‘String’

{- For manually testing parsers:

    parseTest p input

p: a parser
input: input text

-}
main :: IO ()
main = putStrLn "Hello, mum!"

-- Boolean expressions
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp
            BExpr
            BExpr
  | RBinary RBinOp
            AExpr
            AExpr
  deriving (Show)

-- Boolean operators
data BBinOp
  = And
  | Or
  deriving (Show)

-- Relational operators
data RBinOp
  = Greater
  | Less
  deriving (Show)

-- Arithmetic expressions
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp
            AExpr
            AExpr
  deriving (Show)

-- Arithmetic operators
data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

-- Statements
data Stmt
  = Seq [Stmt]
  | Assign String
           AExpr
  | If BExpr
       Stmt
       Stmt
  | While BExpr
          Stmt
  | Skip
  deriving (Show)

--
-- Lexer
--
-- | space consumer.
-- | Consumes whitespace
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Whitespace will be consumed after every lexeme automatically,
-- | but not before it.
-- |
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | We often want to parse some “fixed” string.
-- |
-- | It will take a string as argument and parse this string
-- | and whitespace after it.
-- |
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
-- |
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
-- |
integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.
-- |
semi :: Parser String
semi = symbol ";"

-- | Creates a parser for a reserved word
-- |
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- | Reserved words
-- |
rws :: [String]
rws =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "do"
  , "skip"
  , "true"
  , "false"
  , "not"
  , "and"
  , "or"
  ]

-- | Parse a sequence of characters where first character is a
-- | letter and the rest can be either letters or numbers.
-- |
-- | After parsing a string check if it’s in list of reserved
-- | words, | failing with informative message if it is, returning
-- | the result if it is not.
-- |
-- | Note the use of try in identifier. This is necessary to
-- | backtrack | to beginning of the identifier in cases when
-- | fail is evaluated. Otherwise things like many identifier
-- | would fail on such identifiers | instead of just stopping.
-- |
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-- | Parser for the `while` language.
-- |
-- | Parses whitespace from the start, EOF from the end, and a
-- | statement in the middle.
-- |
whileParser :: Parser Stmt
whileParser = between sc eof stmt

-- | Either a statement surrounded by parens, or a sequence
-- | of statements.
-- |
stmt :: Parser Stmt
stmt = parens stmt <|> stmtSeq

-- | Parse a sequence of statements seperated by semicolons.
-- |
stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt' semi
  -- if there's only one stmt return it without using ‘Seq’
  where
    f l =
      if length l == 1
        then head l
        else Seq l

-- | Parse a single statement.
-- | Try parse an if, then a while, then a skip, then an
-- | assignment.
-- |
-- | (<|>) tries the lhs, then the rhs. lhs consumes nothing
-- | if it fails.
-- |
stmt' :: Parser Stmt
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt

-- | if statement
-- |
ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

-- | while statement
-- |
whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return (While cond stmt1)

-- | assignment statement
-- |
assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

-- | skip statement
-- |
skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

-- | arithmetic expressions
-- |
aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

-- | boolean expressions
-- |
bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

-- | arithmetic operators
-- |
aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-")]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    ]
  , [ InfixL (ABinary Add <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-")
    ]
  ]

-- | boolean operators
-- |
bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not")]
  , [InfixL (BBinary And <$ rword "and"), InfixL (BBinary Or <$ rword "or")]
  ]

-- | Arithmetic terms.
-- |
-- | An arithmetic expression in parens, a variable identifier, or an integer.
-- |
aTerm :: Parser AExpr
aTerm = parens aExpr <|> variable <|> int
  where
    variable = Var <$> identifier
    int = IntConst <$> integer

-- | Boolean terms.
-- |
-- | true, false, or a relational expression
-- |
bTerm :: Parser BExpr
bTerm = parens bExpr <|> true' <|> false' <|> rExpr
  where
    true' = rword "true" *> pure (BoolConst True)
    false' = rword "false" *> pure (BoolConst False)

-- | Relational expression
-- |
-- | i.e. 1 > 2
-- |      x < y
-- |
rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

-- | Greater than or less than
-- |
relation :: Parser RBinOp
relation = gt <|> lt
  where
    gt = symbol ">" *> pure Greater
    lt = symbol "<" *> pure Less
