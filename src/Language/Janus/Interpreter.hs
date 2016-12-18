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

import           Language.Janus.AST


--
-- Errors
--
data EvalError = TypeError
               | InternalError String
               deriving (Eq, Ord)

instance Show EvalError where
  show TypeError           = "type mismatch"
  show (InternalError msg) = "internal error: " ++ msg

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

  eval (NotExpr e') = do
    e <- eval e'
    case e of
      JBool b -> return $ JBool (not b)
      _       -> typee

  eval (BitNotExpr e') = do
    e <- eval e'
    case e of
      JBool b -> return $ JBool (complement b)
      JInt i  -> return $ JInt (complement i)
      _       -> typee

  eval (PlusExpr e') = iie "not implemented yet"

  eval (NegExpr e') = iie "not implemented yet"

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

typee :: InterpM a
typee = throwError TypeError
