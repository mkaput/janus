{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Janus.AST.Val (
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
  tryFromVal
) where

import           Data.Data     (Data, toConstr)
import           Data.Maybe    (fromMaybe)
import           Data.Typeable (TypeRep, Typeable, typeOf)
import           GHC.Float     (double2Float, float2Double)


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
