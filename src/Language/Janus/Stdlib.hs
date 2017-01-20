{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Janus.Stdlib (
  importStdlib,

  retVal,
  throwEx,
  putNativeVar,
  putNativeFunc
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import           Data.Typeable          (TypeRep, Typeable, typeOf)

import           Language.Janus.AST
import           Language.Janus.Interp


-----------------------------------------------------------------------------
--
-- Standard library
--
-----------------------------------------------------------------------------

-- |
-- Import standard library items into current interpreter.
importStdlib :: InterpM ()
importStdlib = do
  putNativeFunc "abs" ["x"] jabs

  putNativeFunc "print" ["v..."] jprint
  putNativeFunc "println" ["v..."] jprintln

  putNativeFunc "getline" [] jGetline

  putNativeFunc "int" ["x"] jToInt

jabs :: [Val] -> InterpM Val
jabs []          = throwEx "no arguments"
jabs (_:_:_)     = throwEx "too many arguments"
jabs [JInt x]    = retVal $ abs x
jabs [JDouble x] = retVal $ abs x
jabs [v]         = throwEx $ "expected number, got " ++ showVal v

jprint :: [Val] -> InterpM Val
jprint []  = retVal ()
jprint [v] = do
  liftIO . putStr . showVal $ v
  retVal ()
jprint (v:vs) = do
  liftIO . putStr . showVal $ v
  vs `forM_` (liftIO . putStr . ('\t':) . showVal)
  retVal ()

jprintln :: [Val] -> InterpM Val
jprintln vs = jprint vs <* liftIO (putStrLn "")

jGetline :: [Val] -> InterpM Val
jGetline [] = liftIO . getLine >>= toVal
jGetline _  = throwEx "unexpected arguments"

jToInt :: [Val] -> InterpM Val
jToInt [v] = retVal . toInteger . fromVal $ v
jToInt []  = throwEx "expected value to convert"
jToInt _   = throwEx "too many arguments"


-----------------------------------------------------------------------------
--
-- Native items creating utils
--
-----------------------------------------------------------------------------

-- |
-- @
--    retVal = return . 'toVal'
-- @
retVal :: ToVal a => a -> InterpM Val
retVal = return . toVal

-- |
-- Throw `CustomError` with given message.
throwEx :: String -> InterpM a
throwEx = throwError . CustomError

-- |
-- Allocate value and bind it to variable.
putNativeVar :: ToVal a => String -> a -> InterpM ()
putNativeVar name nv = malloc (toVal nv) >>= putVar name

-- |
-- Declare native function.
putNativeFunc :: String                 -- ^ Function name
              -> [String]               -- ^ Function parameters names
              -> ([Val] -> InterpM Val) -- ^ Function
              -> InterpM ()
putNativeFunc n p f = putNativeVar n $ NativeFunc n p f
