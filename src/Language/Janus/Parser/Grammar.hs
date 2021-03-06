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
substStmt = try(do
  lval <- lvalue
  reservedOp ":="
  expr <- expression
  semi
  return (SubstStmt lval expr))

lvalue = try(lndexLv <|> path)
--lvalue = try( lndexLv <|> path )

path = identifier >>= return . Path

lndexLv = try(do
  p <- identifier
  e <- brackets (expression)
  return (IndexLv p e))


exprStmt :: Parser Stmt
exprStmt = do
  e <- expression
  optional semi
  return (ExprStmt e)

-----------------------------------------------------------------------------
--
-- Expressions
--
-----------------------------------------------------------------------------
expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"


table = [ [Prefix (reservedOp "!" >> return (NotExpr))]
        , [Prefix (reservedOp "~" >> return (BitNotExpr))]
        , [Infix (reservedOp "**" >> return (ExpExpr)) AssocRight]
        , [Infix (reservedOp "*" >> return (MulExpr)) AssocLeft]
        , [Infix (reservedOp "/" >> return (DivExpr)) AssocLeft]
        , [Infix (keyword "mod" >> return (RemExpr)) AssocLeft]
        , [Infix (reservedOp "+" >> return (AddExpr)) AssocLeft]
        , [Infix (reservedOp "-" >> return (SubExpr)) AssocLeft]
        , [Infix (reservedOp "<<" >> return (LshExpr)) AssocLeft]
        , [Infix (reservedOp ">>" >> return (RshExpr)) AssocLeft]
        , [Infix (reservedOp "&" >> return (BitAndExpr)) AssocLeft]
        , [Infix (reservedOp "^" >> return (BitXorExpr)) AssocLeft]
        , [Infix (reservedOp "|" >> return (BitOrExpr)) AssocLeft]
        , [Infix (reservedOp "==" >> return (EqExpr)) AssocLeft]
        , [Infix (reservedOp "!=" >> return (NeqExpr)) AssocLeft]
        , [Infix (reservedOp "<" >> return (LtExpr)) AssocLeft]
        , [Infix (reservedOp ">" >> return (GtExpr)) AssocLeft]
        , [Infix (reservedOp "<=" >> return (LtEqExpr)) AssocLeft]
        , [Infix (reservedOp ">=" >> return (GtEqExpr)) AssocLeft]
        , [Infix (reservedOp "and" >> return (AndExpr)) AssocLeft]
        , [Infix (reservedOp "or" >> return (OrExpr)) AssocLeft]
        ]


term = literalExpr
      <|> blockExpr
      <|> parenExpr
      <|> callExpr
      <|> postfixIncExpr
      <|> postfixDecExpr
      <|> prefixIncExpr
      <|> prefixDecExpr
      <|> plusExpr
      <|> negExpr
      <|> ifExpr
      <|> whileExpr
      <|> loopExpr
      <|> breakExpr
      <|> continueExpr
      <|> returnExpr
      <|> lvalueExpr


literalExpr :: Parser Expr
literalExpr = literal >>= return . LiteralExpr

blockExpr :: Parser Expr
blockExpr = block >>= return . BlockExpr

parenExpr :: Parser Expr
parenExpr = parens expression >>= return . ParenExpr

callExpr :: Parser Expr
callExpr = try(do
  f <- lvalueExpr
  s <- parens(commaSep expression)
  return (CallExpr f s))

postfixIncExpr :: Parser Expr
postfixIncExpr = try(do
  l <- lvalue
  reservedOp "++"
  return (PostfixIncExpr l))

postfixDecExpr :: Parser Expr
postfixDecExpr = try(do
  l <- lvalue
  reservedOp "--"
  return (PostfixDecExpr l))

prefixIncExpr :: Parser Expr
prefixIncExpr = try(do
  reservedOp "++"
  l <- lvalue
  return (PrefixIncExpr l))

prefixDecExpr :: Parser Expr
prefixDecExpr = try(do
  reservedOp "--"
  l <- lvalue
  return (PrefixDecExpr l))

plusExpr :: Parser Expr
plusExpr = try(do
  reservedOp "+"
  e <- expression
  return (PlusExpr e))

negExpr :: Parser Expr
negExpr = try(do
  reservedOp "-"
  e <- expression
  return (NegExpr e))

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expression
  ifBranch <- blockExpr
  elseBranch <- else'
  return (IfExpr cond ifBranch elseBranch)

else' = (keyword "else" >>(try(ifExpr >>= return . Just)
          <|> try(blockExpr >>= return . Just)))
        <|> (return Nothing)

whileExpr :: Parser Expr
whileExpr = do
  keyword "while"
  cond <- expression
  body <- blockExpr
  return (WhileExpr cond body)

loopExpr :: Parser Expr
loopExpr = keyword "loop" >> blockExpr >>= return . LoopExpr

breakExpr :: Parser Expr
breakExpr = keyword "break" >> return BreakExpr

continueExpr :: Parser Expr
continueExpr = keyword "continue" >> return ContinueExpr

returnExpr :: Parser Expr
returnExpr = keyword "return" >> (try(expression >>= return . ReturnExpr) <|> return (ReturnExpr $ LiteralExpr JUnit))

lvalueExpr :: Parser Expr
lvalueExpr = lvalue >>= return . LvalueExpr
