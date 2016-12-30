{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Janus.Interp (
  module Language.Janus.Interp.Error,
  module Language.Janus.Interp.Monad,

  runInterpM,
  run,

  Evaluable,
  eval
) where

import           Control.Monad.Except        (runExceptT, throwError)
import           Control.Monad.State.Strict  (evalStateT)
import           Data.Bits                   (complement, rotateL, rotateR, xor,
                                              (.&.), (.|.))
import           Data.Typeable               (TypeRep, Typeable, typeOf)

import           Language.Janus.AST
import           Language.Janus.Interp.Error
import           Language.Janus.Interp.Monad


runInterpM :: InterpM a -> IO (Either EvalError a)
runInterpM m = do { st <- emptyState; runExceptT (evalStateT m st) }

run :: Evaluable a => a -> IO (Either EvalError Val)
run = runInterpM . eval


--
-- Evaluable
--
class Evaluable a where
  eval :: a -> InterpM Val

instance Evaluable Val where
  eval = return

instance Evaluable EvalError where
  eval = throwError

instance Evaluable Lvalue where
  eval (IndexLv v' idx') = iie "not implemented yet"

  eval (Path name)       = evalSymbol name

instance Evaluable Expr where
  eval (LiteralExpr a') = eval a'

  eval (BlockExpr block') = iie "not implemented yet"

  eval (ParenExpr e') = eval e'

  eval (CallExpr e' args') = iie "not implemented yet"

  eval (PostfixIncExpr e') = iie "not implemented yet"

  eval (PostfixDecExpr e') = iie "not implemented yet"

  eval (NotExpr e') = callOp1 "!x" [wrapOp1 not] e'

  eval (BitNotExpr e') = callOp1 "~x" [
      wrapOp1 (complement :: Bool -> Bool),
      wrapOp1 (complement :: Integer -> Integer)
    ] e'

  eval (PlusExpr e') = callOp1 "+x" [
      wrapOp1 (id :: Integer -> Integer),
      wrapOp1 (id :: Double -> Double)
    ] e'

  eval (NegExpr e') = callOp1 "-x" [
      wrapOp1 (negate :: Integer -> Integer),
      wrapOp1 (negate :: Double -> Double)
    ] e'

  eval (PrefixIncExpr e') = iie "not implemented yet"

  eval (PrefixDecExpr e') = iie "not implemented yet"

  eval (ExpExpr a' b') = callOp2 "x ** n" [
      wrapOp2 ((^) :: Integer -> Integer -> Integer),
      wrapOp2 ((**) :: Double -> Double -> Double)
    ] a' b'

  eval (MulExpr a' b') = callOp2 "a * b" [
      wrapOp2 ((*) :: Integer -> Integer -> Integer),
      wrapOp2 ((*) :: Double -> Double -> Double)
    ] a' b'

  eval (DivExpr a' b') = callOp2 "a / b" [
      wrapOp2 (div :: Integer -> Integer -> Integer),
      wrapOp2 ((/) :: Double -> Double -> Double)
    ] a' b'

  eval (RemExpr a' b') = callOp2 "a mod b" [
      wrapOp2 (mod :: Integer -> Integer -> Integer)
    ] a' b'

  eval (AddExpr a' b') = callOp2 "a + b" [
      wrapOp2 ((+) :: Integer -> Integer -> Integer),
      wrapOp2 ((+) :: Double -> Double -> Double),
      wrapOp2 ((++) :: String -> String -> String),
      wrapOp2 ((:) :: Char -> String -> String),
      wrapOp2 stringPlusChar
    ] a' b'
    where
      stringPlusChar :: String -> Char -> String
      stringPlusChar str ch = str ++ [ch]

  eval (SubExpr a' b') = callOp2 "a - b" [
      wrapOp2 ((-) :: Integer -> Integer -> Integer),
      wrapOp2 ((-) :: Double -> Double -> Double)
    ] a' b'

  eval (LshExpr a' b') = callOp2 "a << b" [
      wrapOp2 (rotateL :: Integer -> Int -> Integer)
    ] a' b'

  eval (RshExpr a' b') = callOp2 "a >> b" [
      wrapOp2 (rotateR :: Integer -> Int -> Integer)
    ] a' b'

  eval (BitAndExpr a' b') = callOp2 "a & b" [
      wrapOp2 ((.&.) :: Bool -> Bool -> Bool),
      wrapOp2 ((.&.) :: Integer -> Integer -> Integer)
    ] a' b'

  eval (BitXorExpr a' b') = callOp2 "a ^ b" [
      wrapOp2 (xor :: Bool -> Bool -> Bool),
      wrapOp2 (xor :: Integer -> Integer -> Integer)
    ] a' b'

  eval (BitOrExpr a' b') = callOp2 "a | b" [
      wrapOp2 ((.|.) :: Bool -> Bool -> Bool),
      wrapOp2 ((.|.) :: Integer -> Integer -> Integer)
    ] a' b'

  eval (EqExpr a' b') = callOp2 "a == b" [
      wrapOp2 ((==) :: () -> () -> Bool),
      wrapOp2 ((==) :: Bool -> Bool -> Bool),
      wrapOp2 ((==) :: Integer -> Integer -> Bool),
      wrapOp2 ((==) :: Double -> Double -> Bool),
      wrapOp2 ((==) :: Char -> Char -> Bool),
      wrapOp2 ((==) :: String -> String -> Bool)
    ] a' b'

  eval (NeqExpr a' b') = callOp2 "a /= b" [
      wrapOp2 ((/=) :: () -> () -> Bool),
      wrapOp2 ((/=) :: Bool -> Bool -> Bool),
      wrapOp2 ((/=) :: Integer -> Integer -> Bool),
      wrapOp2 ((/=) :: Double -> Double -> Bool),
      wrapOp2 ((/=) :: Char -> Char -> Bool),
      wrapOp2 ((/=) :: String -> String -> Bool)
    ] a' b'

  eval (LtExpr a' b') = callOp2 "a < b" [
      wrapOp2 ((<) :: () -> () -> Bool),
      wrapOp2 ((<) :: Bool -> Bool -> Bool),
      wrapOp2 ((<) :: Integer -> Integer -> Bool),
      wrapOp2 ((<) :: Double -> Double -> Bool),
      wrapOp2 ((<) :: Char -> Char -> Bool),
      wrapOp2 ((<) :: String -> String -> Bool)
    ] a' b'

  eval (GtExpr a' b') = callOp2 "a > b" [
      wrapOp2 ((>) :: () -> () -> Bool),
      wrapOp2 ((>) :: Bool -> Bool -> Bool),
      wrapOp2 ((>) :: Integer -> Integer -> Bool),
      wrapOp2 ((>) :: Double -> Double -> Bool),
      wrapOp2 ((>) :: Char -> Char -> Bool),
      wrapOp2 ((>) :: String -> String -> Bool)
    ] a' b'

  eval (LtEqExpr a' b') = callOp2 "a <= b" [
      wrapOp2 ((<=) :: () -> () -> Bool),
      wrapOp2 ((<=) :: Bool -> Bool -> Bool),
      wrapOp2 ((<=) :: Integer -> Integer -> Bool),
      wrapOp2 ((<=) :: Double -> Double -> Bool),
      wrapOp2 ((<=) :: Char -> Char -> Bool),
      wrapOp2 ((<=) :: String -> String -> Bool)
    ] a' b'

  eval (GtEqExpr a' b') = callOp2 "a >= b" [
      wrapOp2 ((>=) :: () -> () -> Bool),
      wrapOp2 ((>=) :: Bool -> Bool -> Bool),
      wrapOp2 ((>=) :: Integer -> Integer -> Bool),
      wrapOp2 ((>=) :: Double -> Double -> Bool),
      wrapOp2 ((>=) :: Char -> Char -> Bool),
      wrapOp2 ((>=) :: String -> String -> Bool)
    ] a' b'

  eval (AndExpr a' b') = callOp2 "a and b" [
      wrapOp2 ((&&) :: Bool -> Bool -> Bool)
    ] a' b'

  eval (OrExpr a' b') = callOp2 "a or b" [
      wrapOp2 ((||) :: Bool -> Bool -> Bool)
    ] a' b'

  eval IfExpr{cond=cond', ifBranch=a', elseBranch=b'} = iie "not implemented yet"

  eval WhileExpr{cond=cond', body=body'} = iie "not implemented yet"

  eval (LoopExpr e') = iie "not implemented yet"

  eval BreakExpr = iie "not implemented yet"

  eval ContinueExpr = iie "not implemented yet"

  eval (ReturnExpr e') = iie "not implemented yet"

  eval (LvalueExpr lv) = eval lv


--
-- Misc
--
wrapOp1 :: forall a b. (FromVal a, ToVal b) => (a -> b) -> (Val -> Maybe Val, [TypeRep])
wrapOp1 f = (
    fmap (toVal . f) . tryFromVal,
    [typeOf (undefined :: a), typeOf (undefined :: b)]
  )

callOp1 :: Evaluable a => String -> [(Val -> Maybe Val, [TypeRep])] -> a -> InterpM Val
callOp1 opName fs a' = do
  a <- eval a'
  doCall fs a []
    where
      doCall :: [(Val -> Maybe Val, [TypeRep])] -> Val -> [[TypeRep]] -> InterpM Val
      doCall [] a triedSigs = throwError OpCallTypeError {
          opName = opName,
          triedSigs = triedSigs,
          givenSig = [haskellTypeRep a]
        }
      doCall ((f, sig):fs) a triedSigs = case f a of
        Just v  -> return v
        Nothing -> doCall fs a (sig:triedSigs)

wrapOp2 :: forall a b c. (FromVal a, FromVal b, ToVal c)
  => (a -> b -> c) -> (Val -> Val -> Maybe Val, [TypeRep])
wrapOp2 f = (
    \a' b' -> do
      a <- tryFromVal a'
      b <- tryFromVal b'
      return . toVal $ f a b,
    [typeOf (undefined :: a), typeOf (undefined :: b), typeOf (undefined :: c)]
  )

callOp2 :: (Evaluable a, Evaluable b)
  => String -> [(Val -> Val -> Maybe Val, [TypeRep])] -> a -> b -> InterpM Val
callOp2 opName fs a' b' = do
  a <- eval a'
  b <- eval b'
  doCall fs a b []
    where
      doCall :: [(Val -> Val -> Maybe Val, [TypeRep])] -> Val -> Val -> [[TypeRep]] -> InterpM Val
      doCall [] a b triedSigs = throwError OpCallTypeError {
          opName = opName,
          triedSigs = triedSigs,
          givenSig = [haskellTypeRep a, haskellTypeRep b]
        }
      doCall ((f, sig):fs) a b triedSigs = case f a b of
        Just v  -> return v
        Nothing -> doCall fs a b (sig:triedSigs)
