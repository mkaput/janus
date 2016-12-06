module Language.Janus.AST where

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

data Expr = LiteralExpr
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
              ifCond :: Expr,
              ifBranch :: Block,
              elseBranch :: Block
            }
          | ForExpr {
              forInit :: Maybe LetDecl,
              forCond :: Maybe Expr,
              forAfterthought :: Maybe Expr
            }
          | WhileExpr {
              whileCond :: Expr,
              whileBody :: Block
            }
          | LoopExpr Block

          | BreakExpr
          | ContinueExpr
          | ReturnExpr Expr
          deriving (Show, Eq)

data LetDecl = LetDecl
             deriving (Show, Eq)

data Block = Block {
    blockStmts :: [Stmt],
    blockExpr :: Maybe Expr
  }
  deriving (Show, Eq)

data Stmt = Stmt
  deriving (Show, Eq)
