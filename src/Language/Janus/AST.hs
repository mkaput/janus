{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Janus.AST where

import           Data.Data     (Data, toConstr)
import           Data.Maybe    (fromMaybe)
import           Data.Typeable (TypeRep, Typeable, typeOf)
import           GHC.Float     (double2Float, float2Double)


--
-- Tokens
--
newtype Ident = Ident String
              deriving (Show, Eq, Ord)


--
-- Val
--
data Val = JUnit
         | JBool Bool
         | JInt Integer
         | JDouble Double
         | JChar Char
         | JStr String
         deriving (Show, Eq, Ord, Data, Typeable)

showVal :: Val -> String
showVal JUnit       = "()"
showVal (JBool x)   = show x
showVal (JInt x)    = show x
showVal (JDouble x) = show x
showVal (JChar x)   = show x
showVal (JStr x)    = show x

haskellTypeRep :: Val -> TypeRep
haskellTypeRep JUnit       = typeOf ()
haskellTypeRep (JBool a)   = typeOf a
haskellTypeRep (JInt a)    = typeOf a
haskellTypeRep (JDouble a) = typeOf a
haskellTypeRep (JChar a)   = typeOf a
haskellTypeRep (JStr a)    = typeOf a


--
-- ToVal class
--
class Typeable a => ToVal a where
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

toValI :: Integral a => a -> Val
toValI = toVal . toInteger

toValF :: Float -> Val
toValF = toVal . float2Double

toValD :: Double -> Val
toValD = toVal

toLiteralI :: Integral a => a -> Expr
toLiteralI = LiteralExpr . toValI

toLiteralF :: Float -> Expr
toLiteralF = LiteralExpr . toValF

toLiteralD :: Double -> Expr
toLiteralD = LiteralExpr . toValD


--
-- FromVal class
--
class Typeable a => FromVal a where
  fromVal :: Val -> a
  fromVal a = fromMaybe (
      error $
        "Failed to convert Janus value of type " ++ show (toConstr a)
        ++ " to Haskell value of type " ++ show (typeOf (undefined :: a))
    ) (tryFromVal a)

  tryFromVal :: Val -> Maybe a

instance FromVal () where
  tryFromVal JUnit = Just ()
  tryFromVal _     = Nothing

instance FromVal Bool where
  tryFromVal (JBool b) = Just b
  tryFromVal _         = Nothing

instance FromVal Integer where
  tryFromVal (JInt b) = Just b
  tryFromVal _        = Nothing

instance FromVal Int where
  tryFromVal (JInt b) = Just . fromInteger $ b
  tryFromVal _        = Nothing

instance FromVal Float where
  tryFromVal (JDouble d) = Just . double2Float $ d
  tryFromVal _           = Nothing

instance FromVal Double where
  tryFromVal (JDouble d) = Just d
  tryFromVal _           = Nothing

instance FromVal Char where
  tryFromVal (JChar c) = Just c
  tryFromVal _         = Nothing

instance FromVal String where
  tryFromVal (JStr s) = Just s
  tryFromVal _        = Nothing


--
-- Expressions & statements
--
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
