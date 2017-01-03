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

importStdlib :: InterpM ()
importStdlib = do
  putNativeFunc "abs" ["x"] jabs

  putNativeFunc "print" ["v..."] jprint
  putNativeFunc "println" ["v..."] jprintln

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


-----------------------------------------------------------------------------
--
-- Native items creating utils
--
-----------------------------------------------------------------------------

retVal :: ToVal a => a -> InterpM Val
retVal = return . toVal

throwEx :: String -> InterpM a
throwEx = throwError . CustomError

putNativeVar :: ToVal a => String -> a -> InterpM ()
putNativeVar name nv = malloc (toVal nv) >>= putVar name

putNativeFunc :: String -> [String] -> ([Val] -> InterpM Val) -> InterpM ()
putNativeFunc n p f = putNativeVar n $ NativeFunc n p f
