module Language.Janus.Parser.Grammar (
  statement,
  expression
) where

import           Language.Janus.AST
import           Language.Janus.Parser.Lexer

-----------------------------------------------------------------------------
--
-- Statements
--
-----------------------------------------------------------------------------
statement :: Parser Stmt
statement = letDecl
          <|> fnDecl
          <|> substStmt
          <|> exprStmt
          <?> "statement"

letDecl :: Parser Stmt
letDecl = undefined

fnDecl :: Parser Stmt
fnDecl = undefined

substStmt :: Parser Stmt
substStmt = undefined

exprStmt :: Parser Stmt
exprStmt = undefined

-----------------------------------------------------------------------------
--
-- Expressions
--
-----------------------------------------------------------------------------
expression :: Parser Expr
expression =  literalExpr
          <|> blockExpr
          <|> parenExpr
          <|> callExpr
          <|> postfixIncExpr
          <|> postfixDecExpr
          <|> notExpr
          <|> bitNotExpr
          <|> plusExpr
          <|> negExpr
          <|> prefixIncExpr
          <|> prefixDecExpr
          <|> expExpr
          <|> mulExpr
          <|> divExpr
          <|> remExpr
          <|> addExpr
          <|> subExpr
          <|> lshExpr
          <|> rshExpr
          <|> bitAndExpr
          <|> bitXorExpr
          <|> bitOrExpr
          <|> eqExpr
          <|> neqExpr
          <|> ltExpr
          <|> gtExpr
          <|> ltEqExpr
          <|> gtEqExpr
          <|> andExpr
          <|> orExpr
          <|> ifExpr
          <|> whileExpr
          <|> loopExpr
          <|> breakExpr
          <|> continueExpr
          <|> returnExpr
          <|> lvalueExpr
          <?> "expression"

literalExpr :: Parser Stmt
literalExpr = undefined

blockExpr :: Parser Stmt
blockExpr = undefined

parenExpr :: Parser Stmt
parenExpr = undefined

callExpr :: Parser Stmt
callExpr = undefined

postfixIncExpr :: Parser Stmt
postfixIncExpr = undefined

postfixDecExpr :: Parser Stmt
postfixDecExpr = undefined

notExpr :: Parser Stmt
notExpr = undefined

bitNotExpr :: Parser Stmt
bitNotExpr = undefined

plusExpr :: Parser Stmt
plusExpr = undefined

negExpr :: Parser Stmt
negExpr = undefined

prefixIncExpr :: Parser Stmt
prefixIncExpr = undefined

prefixDecExpr :: Parser Stmt
prefixDecExpr = undefined

expExpr :: Parser Stmt
expExpr = undefined

mulExpr :: Parser Stmt
mulExpr = undefined

divExpr :: Parser Stmt
divExpr = undefined

remExpr :: Parser Stmt
remExpr = undefined

addExpr :: Parser Stmt
addExpr = undefined

subExpr :: Parser Stmt
subExpr = undefined

lshExpr :: Parser Stmt
lshExpr = undefined

rshExpr :: Parser Stmt
rshExpr = undefined

bitAndExpr :: Parser Stmt
bitAndExpr = undefined

bitXorExpr :: Parser Stmt
bitXorExpr = undefined

bitOrExpr :: Parser Stmt
bitOrExpr = undefined

eqExpr :: Parser Stmt
eqExpr = undefined

neqExpr :: Parser Stmt
neqExpr = undefined

ltExpr :: Parser Stmt
ltExpr = undefined

gtExpr :: Parser Stmt
gtExpr = undefined

ltEqExpr :: Parser Stmt
ltEqExpr = undefined

gtEqExpr :: Parser Stmt
gtEqExpr = undefined

andExpr :: Parser Stmt
andExpr = undefined

orExpr :: Parser Stmt
orExpr = undefined

ifExpr :: Parser Stmt
ifExpr = undefined

whileExpr :: Parser Stmt
whileExpr = undefined

loopExpr :: Parser Stmt
loopExpr = undefined

breakExpr :: Parser Stmt
breakExpr = undefined

continueExpr :: Parser Stmt
continueExpr = undefined

returnExpr :: Parser Stmt
returnExpr = undefined

lvalueExpr :: Parser Stmt
lvalueExpr = undefined
