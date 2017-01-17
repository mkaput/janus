module Language.Janus.Parser (
  parseProgram,
  parseStatement,
  parseExpression,
  parseLiteral
) where

import           Control.Applicative               (many, (*>), (<$), (<$>),
                                                    (<*), (<*>), (<|>))

import           Text.Parsec                       (ParseError, eof, parse)

import           Language.Janus.AST
import           Language.Janus.Parser.Expressions
import           Language.Janus.Parser.Lexer
import           Language.Janus.Parser.Statements

-- |
-- Parse whole Janus program source code.
parseProgram :: String -- ^ File name (used in error messages)
             -> String -- ^ Source code
             -> Either ParseError Program
parseProgram = parse (Program <$> (whiteSpace *> many statement <* eof))

-- |
-- Parse single statement
parseStatement :: String -- ^ Source code
               -> Either ParseError Stmt
parseStatement = parse (whiteSpace *> statement <* eof) ""

-- |
-- Parse single expression
parseExpression :: String -- ^ Source code
                -> Either ParseError Expr
parseExpression = parse (whiteSpace *> expression <* eof) ""

-- |
-- Parse single literal
parseLiteral :: String -- ^ Source code
             -> Either ParseError Val
parseLiteral = parse (whiteSpace *> literal <* eof) ""
