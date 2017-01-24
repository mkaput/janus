module Language.Janus.Parser.Grammar (
  statement,
  expression
) where


import           Language.Janus.AST
import           Language.Janus.Parser.Lexer
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr

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
letDecl = do
  keyword "let"
  ident <- identifier
  reservedOp "="
  expr <- expression
  semi
  return (LetDecl ident expr)

fnDecl :: Parser Stmt
fnDecl = do
  keyword "fn"
  ident <- identifier
  params <- parens(commaSep identifier)
  b <- block
  optional semi
  return (FnDecl ident params b)

block = braces (many statement) >>= return . Block

substStmt :: Parser Stmt
substStmt = do
  lval <- lvalue
  reservedOp ":="
  expr <- expression
  semi
  return (SubstStmt lval expr)

lvalue = lndexLv
        <|> path

path = identifier >>= return . Path

lndexLv = do
  p <- identifier
  e <- expression
  return (IndexLv p e)

exprStmt :: Parser Stmt
exprStmt = do
  e <- expression
  semi
  return (ExprStmt e)

-----------------------------------------------------------------------------
--
-- Expressions
--
-----------------------------------------------------------------------------
expression :: Parser Expr
expression = literalExpr
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

literalExpr :: Parser Expr
literalExpr = undefined

blockExpr :: Parser Expr
blockExpr = undefined

parenExpr :: Parser Expr
parenExpr = undefined

callExpr :: Parser Expr
callExpr = undefined

postfixIncExpr :: Parser Expr
postfixIncExpr = undefined

postfixDecExpr :: Parser Expr
postfixDecExpr = undefined

notExpr :: Parser Expr
notExpr = undefined

bitNotExpr :: Parser Expr
bitNotExpr = undefined

plusExpr :: Parser Expr
plusExpr = undefined

negExpr :: Parser Expr
negExpr = undefined

prefixIncExpr :: Parser Expr
prefixIncExpr = undefined

prefixDecExpr :: Parser Expr
prefixDecExpr = undefined

expExpr :: Parser Expr
expExpr = undefined

mulExpr :: Parser Expr
mulExpr = undefined

divExpr :: Parser Expr
divExpr = undefined

remExpr :: Parser Expr
remExpr = undefined

addExpr :: Parser Expr
addExpr = undefined

subExpr :: Parser Expr
subExpr = undefined

lshExpr :: Parser Expr
lshExpr = undefined

rshExpr :: Parser Expr
rshExpr = undefined

bitAndExpr :: Parser Expr
bitAndExpr = undefined

bitXorExpr :: Parser Expr
bitXorExpr = undefined

bitOrExpr :: Parser Expr
bitOrExpr = undefined

eqExpr :: Parser Expr
eqExpr = undefined

neqExpr :: Parser Expr
neqExpr = undefined

ltExpr :: Parser Expr
ltExpr = undefined

gtExpr :: Parser Expr
gtExpr = undefined

ltEqExpr :: Parser Expr
ltEqExpr = undefined

gtEqExpr :: Parser Expr
gtEqExpr = undefined

andExpr :: Parser Expr
andExpr = undefined

orExpr :: Parser Expr
orExpr = undefined

ifExpr :: Parser Expr
ifExpr = undefined

whileExpr :: Parser Expr
whileExpr = undefined

loopExpr :: Parser Expr
loopExpr = undefined

breakExpr :: Parser Expr
breakExpr = undefined

continueExpr :: Parser Expr
continueExpr = undefined

returnExpr :: Parser Expr
returnExpr = undefined

lvalueExpr :: Parser Expr
lvalueExpr = undefined
