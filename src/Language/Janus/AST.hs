module Language.Janus.AST (
  Val(..),
  showVal
) where

data Val = JBool Bool
         | JInt Integer
         | JDouble Double
         | JChar Char
         | JStr String
         deriving (Show, Eq, Ord)

showVal :: Val -> String
showVal (JBool x)   = show x
showVal (JInt x)    = show x
showVal (JDouble x) = show x
showVal (JChar x)   = show x
showVal (JStr x)    = show x
