{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Janus.AST (
  Program(..),

  Ptr(..),
  getAddress,

  Ref(..),

  Val(..),
  showVal,
  haskellTypeRep,

  ToVal,
  toVal,
  toValI,
  toValF,
  toValD,

  FromVal,
  fromVal,
  tryFromVal,

  toLiteral,
  toLiteralI,
  toLiteralF,
  toLiteralD,

  Lvalue(..),
  Expr(..),
  Block(..),
  Stmt(..),
  Item(..)
) where

import           Data.Data             (Data, toConstr)
import           Data.Maybe            (fromMaybe)
import           Data.Typeable         (TypeRep, Typeable, typeOf)
import           GHC.Float             (double2Float, float2Double)

import           Data.Hashable         (Hashable, hash, hashWithSalt)

-- BEWARE!!! Stylish Haskell hates {-# SOURCE #-} pragma and removes it
-- Check out: https://github.com/jaspervdj/stylish-haskell/pull/143
import {-# SOURCE #-} Language.Janus.Interp (InterpM)

newtype Program = Program [Stmt]

-----------------------------------------------------------------------------
--
-- Pointer
--
-----------------------------------------------------------------------------

newtype Ptr = Ptr Word
            deriving (Eq, Ord, Show, Data, Typeable)

instance Bounded Ptr where
  minBound = Ptr minBound
  maxBound = Ptr maxBound

instance Hashable Ptr where
  hashWithSalt s (Ptr p) = hashWithSalt s p
  hash = hash . getAddress

getAddress :: Ptr -> Word
getAddress (Ptr p) = p


-----------------------------------------------------------------------------
--
-- Ref
--
-----------------------------------------------------------------------------

data Ref = PtrRef Ptr
         | IndexRef Ptr Val
         deriving (Show, Eq, Ord)


-----------------------------------------------------------------------------
--
-- Val
--
-----------------------------------------------------------------------------

data Val = JUnit
         | JBool Bool
         | JInt Integer
         | JDouble Double
         | JChar Char
         | JStr String
         | JItem Item
         deriving (Show, Eq, Ord)

showVal :: Val -> String
showVal JUnit       = "()"
showVal (JBool x)   = show x
showVal (JInt x)    = show x
showVal (JDouble x) = show x
showVal (JChar x)   = show x
showVal (JStr x)    = show x
showVal (JItem x)   = show x

haskellTypeRep :: Val -> TypeRep
haskellTypeRep JUnit       = typeOf ()
haskellTypeRep (JBool a)   = typeOf a
haskellTypeRep (JInt a)    = typeOf a
haskellTypeRep (JDouble a) = typeOf a
haskellTypeRep (JChar a)   = typeOf a
haskellTypeRep (JStr a)    = typeOf a
haskellTypeRep (JItem a)   = typeOf a


-----------------------------------------------------------------------------
--
-- ToVal
--
-----------------------------------------------------------------------------

class ToVal a where
  toVal :: a -> Val

instance ToVal Val where
  toVal = id

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

toValI :: Integral a => a -> Val
toValI = toVal . toInteger

toValF :: Float -> Val
toValF = toVal . float2Double

toValD :: Double -> Val
toValD = toVal


-----------------------------------------------------------------------------
--
-- FromVal
--
-----------------------------------------------------------------------------

class FromVal a where
  fromVal :: Val -> a
  fromVal a = fromMaybe
    (error "Failed to convert Janus value to Haskell value")
    (tryFromVal a)

  tryFromVal :: Val -> Maybe a
  tryFromVal = Just . fromVal

  {-# MINIMAL fromVal | tryFromVal #-}

instance FromVal Val where
  fromVal = id

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


-----------------------------------------------------------------------------
--
-- toLiteral
--
-----------------------------------------------------------------------------

toLiteral :: ToVal a => a -> Expr
toLiteral = LiteralExpr . toVal

toLiteralI :: Integral a => a -> Expr
toLiteralI = LiteralExpr . toValI

toLiteralF :: Float -> Expr
toLiteralF = LiteralExpr . toValF

toLiteralD :: Double -> Expr
toLiteralD = LiteralExpr . toValD


-----------------------------------------------------------------------------
--
-- Lvalues
--
-----------------------------------------------------------------------------

data Lvalue = Path String
            | IndexLv String Expr
            deriving (Show, Eq)


-----------------------------------------------------------------------------
--
-- Expressions & statements
--
-----------------------------------------------------------------------------

data Expr = LiteralExpr Val
          | BlockExpr Block

          | ParenExpr Expr

          | CallExpr Expr [Expr]

          | PostfixIncExpr Lvalue
          | PostfixDecExpr Lvalue

          | NotExpr Expr
          | BitNotExpr Expr
          | PlusExpr Expr
          | NegExpr Expr
          | PrefixIncExpr Lvalue
          | PrefixDecExpr Lvalue

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
              elseBranch :: Maybe Expr
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


newtype Block = Block [Stmt]
              deriving (Show, Eq)


data Stmt = LetDecl String Expr
          | FnDecl String [String] Block
          | SubstStmt Lvalue Expr
          | ExprStmt Expr
          deriving (Show, Eq)


-----------------------------------------------------------------------------
--
-- Items
--
-----------------------------------------------------------------------------

data Item = Func String [String] Block
          | NativeFunc String [String] ([Val] -> InterpM Val)

instance Show Item where
  show (Func n p _)       = showFunc "func" n p
  show (NativeFunc n p _) = showFunc "native func" n p

instance Eq Item where
  (==) = error "items are not comparable"

instance Ord Item where
  compare = error "items are not comparable"


-----------------------------------------------------------------------------
--
-- Utils
--
-----------------------------------------------------------------------------

showFunc k n p = "<<" ++ show k
              ++ " " ++ show n
              ++ "(" ++ (foldl1 (\a b -> a ++ ", " ++ b) . fmap show $ p)
              ++ ")>>"
