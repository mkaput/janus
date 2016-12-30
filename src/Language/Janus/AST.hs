{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Language.Janus.AST (
  module Language.Janus.AST.Val,

  toLiteral,
  toLiteralI,
  toLiteralF,
  toLiteralD,

  Ident(..),
  Lvalue(..),
  Expr(..),
  LetDecl(..),
  FnDecl(..),
  Block(..),
  Stmt(..)
) where

import           Language.Janus.AST.Val


toLiteral :: ToVal a => a -> Expr
toLiteral = LiteralExpr . toVal

toLiteralI :: Integral a => a -> Expr
toLiteralI = LiteralExpr . toValI

toLiteralF :: Float -> Expr
toLiteralF = LiteralExpr . toValF

toLiteralD :: Double -> Expr
toLiteralD = LiteralExpr . toValD


--
-- Tokens
--
newtype Ident = Ident String
              deriving (Show, Eq, Ord)


--
-- Lvalues
--
data Lvalue = Path Ident
            | IndexLv Expr Expr
            deriving (Show, Eq)


--
-- Expressions & statements
--
data Expr = LiteralExpr Val
          | BlockExpr Block

          | ParenExpr Expr

          | CallExpr Expr [Expr]

          | PostfixIncExpr Expr
          | PostfixDecExpr Expr

          | NotExpr Expr
          | BitNotExpr Expr
          | PlusExpr Expr
          | NegExpr Expr
          | PrefixIncExpr Expr
          | PrefixDecExpr Expr

          | ExpExpr Expr Expr

          | MulExpr Expr Expr
          | DivExpr Expr Expr
          | RemExpr Expr Expr

          | AddExpr Expr Expr
          | SubExpr Expr Expr

          | LshExpr Expr Expr
          | RshExpr Expr Expr

          | BitAndExpr Expr Expr

          | BitXorExpr Expr Expr

          | BitOrExpr Expr Expr

          | EqExpr Expr Expr
          | NeqExpr Expr Expr
          | LtExpr Expr Expr
          | GtExpr Expr Expr
          | LtEqExpr Expr Expr
          | GtEqExpr Expr Expr

          | AndExpr Expr Expr

          | OrExpr Expr Expr

          | IfExpr {
              cond       :: Expr,
              ifBranch   :: Expr,
              elseBranch :: Expr
            }
          | WhileExpr {
              cond :: Expr,
              body :: Expr
            }
          | LoopExpr Expr

          | BreakExpr
          | ContinueExpr
          | ReturnExpr Expr

          | LvalueExpr Lvalue
          deriving (Show, Eq)


data LetDecl = LetDecl Ident Expr
             deriving (Show, Eq)


data FnDecl = FnDecl {
    name   :: Ident,
    params :: [Ident],
    body   :: Block
  }
  deriving (Show, Eq)


newtype Block = Block [Stmt]
              deriving (Show, Eq)


data Stmt = LetDeclStmt LetDecl
          | FnDeclStmt FnDecl
          | ExprStmt Expr
          deriving (Show, Eq)
