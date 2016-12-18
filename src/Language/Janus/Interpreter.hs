{-# LANGUAGE ScopedTypeVariables #-}

module Language.Janus.Interpreter (
  EvalError(..),

  EvalState(..),
  emptyState,
  InterpM,

  Evaluable,
  eval
) where

import           Control.Exception          (Exception)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Data.Bits                  (complement, xor, (.&.), (.|.))
import           Data.Typeable              (TypeRep, Typeable, typeOf)

import           Language.Janus.AST


--
-- Errors
--
data EvalError = OpCallTypeError {
                  opName    :: String,
                  triedSigs :: [[TypeRep]],
                  givenSig  :: [TypeRep]
                }
               | InternalError String
               deriving (Eq, Ord)

instance Show EvalError where
  show OpCallTypeError{opName=opName, triedSigs=ts, givenSig=gs} =
    "Type mismatch when calling " ++ opName
      ++ "\n  Tried to evaluate: " ++ got
      ++ "\n  But it has following overloads:\n" ++ expected
    where
      expected = foldl1 (\a b -> a ++ ",\n" ++ b)
               . map ((("    " ++ opName ++ ": ") ++) . joinTypes)
               $ ts
      got = opName ++ ": (" ++ joinArgTypes gs ++ ") -> ???"

      joinTypes ls = let
          args = init ls;
          ret = last ls
        in "(" ++ joinArgTypes args ++ ") -> " ++ show ret
      joinArgTypes = foldl1 (\a b -> a ++ ", " ++ b) . fmap show

  show (InternalError msg) = "Internal error: " ++ msg

instance Exception EvalError


--
-- Interpreter Monad
--
data EvalState = EvalState
               deriving (Eq, Show)

emptyState :: EvalState
emptyState = EvalState

type InterpM = StateT EvalState (ExceptT EvalError IO)


--
-- Evaluable
--
class Evaluable a where
  eval :: a -> InterpM Val

instance Evaluable Val where
  eval = return

instance Evaluable EvalError where
  eval = throwError

instance Evaluable Expr where
  eval (LiteralExpr a') = eval a'

  eval (BlockExpr block') = iie "not implemented yet"

  eval (ParenExpr e') = eval e'

  eval (IndexExpr e' idx') = iie "not implemented yet"

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

  eval (ExpExpr a' b') = iie "not implemented yet"

  eval (MulExpr a' b') = iie "not implemented yet"

  eval (DivExpr a' b') = iie "not implemented yet"

  eval (RemExpr a' b') = iie "not implemented yet"

  eval (AddExpr a' b') = iie "not implemented yet"

  eval (SubExpr a' b') = iie "not implemented yet"

  eval (LshExpr a' b') = iie "not implemented yet"

  eval (RshExpr a' b') = iie "not implemented yet"

  eval (BitAndExpr a' b') = iie "not implemented yet"

  eval (BitXorExpr a' b') = iie "not implemented yet"

  eval (BitOrExpr a' b') = iie "not implemented yet"

  eval (EqExpr a' b') = iie "not implemented yet"

  eval (NeqExpr a' b') = iie "not implemented yet"

  eval (LtExpr a' b') = iie "not implemented yet"

  eval (GtExpr a' b') = iie "not implemented yet"

  eval (LtEqExpr a' b') = iie "not implemented yet"

  eval (GtEqExpr a' b') = iie "not implemented yet"

  eval (AndExpr a' b') = iie "not implemented yet"

  eval (OrExpr a' b') = iie "not implemented yet"

  eval IfExpr{ifCond=cond', ifBranch=a', elseBranch=b'} = iie "not implemented yet"

  eval WhileExpr{whileCond=cond', whileBody=body'} = iie "not implemented yet"

  eval (LoopExpr e') = iie "not implemented yet"

  eval BreakExpr = iie "not implemented yet"

  eval ContinueExpr = iie "not implemented yet"

  eval (ReturnExpr e') = iie "not implemented yet"


--
-- Misc
--
iie :: String -> InterpM a
iie = throwError . InternalError

retv :: ToVal a => a -> InterpM Val
retv = return . toVal

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
