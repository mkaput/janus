{-# LANGUAGE FlexibleInstances #-}

module Language.Janus.AST where

import           GHC.Float (float2Double)


newtype Ident = Ident String
              deriving (Show, Eq, Ord)


data Val = JUnit
         | JBool Bool
         | JInt Integer
         | JDouble Double
         | JChar Char
         | JStr String
         deriving (Show, Eq, Ord)

showVal :: Val -> String
showVal JUnit       = "()"
showVal (JBool x)   = show x
showVal (JInt x)    = show x
showVal (JDouble x) = show x
showVal (JChar x)   = show x
showVal (JStr x)    = show x

class ToVal a where
  toVal :: a -> Val

instance ToVal () where
  toVal _ = JUnit

instance ToVal Bool where
  toVal = JBool

instance ToVal Integer where
  toVal = JInt

instance ToVal Int where
  toVal = toVal . toInteger

instance ToVal Double where
  toVal = JDouble

instance ToVal Float where
  toVal = JDouble . float2Double

instance ToVal Char where
  toVal = JChar

instance ToVal String where
  toVal = JStr

toLiteral :: ToVal a => a -> Expr
toLiteral = LiteralExpr . toVal


data Expr = LiteralExpr Val
          | BlockExpr Block

          | ParenExpr Expr

          | IndexExpr Expr Expr
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
              ifCond     :: Expr,
              ifBranch   :: Expr,
              elseBranch :: Expr
            }
          | WhileExpr {
              whileCond :: Expr,
              whileBody :: Expr
            }
          | LoopExpr Expr

          | BreakExpr
          | ContinueExpr
          | ReturnExpr Expr
          deriving (Show, Eq)


data LetDecl = LetDecl Ident Expr
             deriving (Show, Eq)


data FnDecl = FnDecl {
    fnName   :: Ident,
    fnParams :: [Ident],
    fnBody   :: Block
  }
  deriving (Show, Eq)


newtype Block = Block [Stmt]
              deriving (Show, Eq)


data Stmt = LetDeclStmt LetDecl
          | FnDeclStmt FnDecl
          | ExprStmt Expr
          deriving (Show, Eq)
