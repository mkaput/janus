module Language.Janus.Interpreter (
  EvalError,

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

instance Evaluable Expr where
  eval (LiteralExpr a) = eval a

  eval (BlockExpr block) = throwError $ InternalError "not implemented yet"

  eval (ParenExpr e) = eval e

  eval (IndexExpr e idx) = throwError $ InternalError "not implemented yet"

  eval (CallExpr e args) = throwError $ InternalError "not implemented yet"

  eval (PostfixIncExpr e) = throwError $ InternalError "not implemented yet"

  eval (PostfixDecExpr e) = throwError $ InternalError "not implemented yet"

  eval (NotExpr e) = throwError $ InternalError "not implemented yet"

  eval (BitNotExpr e) = throwError $ InternalError "not implemented yet"

  eval (PlusExpr e) = throwError $ InternalError "not implemented yet"

  eval (NegExpr e) = throwError $ InternalError "not implemented yet"

  eval (PrefixIncExpr e) = throwError $ InternalError "not implemented yet"

  eval (PrefixDecExpr e) = throwError $ InternalError "not implemented yet"

  eval (ExpExpr a b) = throwError $ InternalError "not implemented yet"

  eval (MulExpr a b) = throwError $ InternalError "not implemented yet"

  eval (DivExpr a b) = throwError $ InternalError "not implemented yet"

  eval (RemExpr a b) = throwError $ InternalError "not implemented yet"

  eval (AddExpr a b) = throwError $ InternalError "not implemented yet"

  eval (SubExpr a b) = throwError $ InternalError "not implemented yet"

  eval (LshExpr a b) = throwError $ InternalError "not implemented yet"

  eval (RshExpr a b) = throwError $ InternalError "not implemented yet"

  eval (BitAndExpr a b) = throwError $ InternalError "not implemented yet"

  eval (BitXorExpr a b) = throwError $ InternalError "not implemented yet"

  eval (BitOrExpr a b) = throwError $ InternalError "not implemented yet"

  eval (EqExpr a b) = throwError $ InternalError "not implemented yet"

  eval (NeqExpr a b) = throwError $ InternalError "not implemented yet"

  eval (LtExpr a b) = throwError $ InternalError "not implemented yet"

  eval (GtExpr a b) = throwError $ InternalError "not implemented yet"

  eval (LtEqExpr a b) = throwError $ InternalError "not implemented yet"

  eval (GtEqExpr a b) = throwError $ InternalError "not implemented yet"

  eval (AndExpr a b) = throwError $ InternalError "not implemented yet"

  eval (OrExpr a b) = throwError $ InternalError "not implemented yet"

  eval IfExpr{ifCond=cond, ifBranch=a, elseBranch=b} =
    throwError $ InternalError "not implemented yet"

  eval WhileExpr{whileCond=cond, whileBody=body} =
    throwError $ InternalError "not implemented yet"

  eval (LoopExpr e) = throwError $ InternalError "not implemented yet"

  eval BreakExpr = throwError $ InternalError "not implemented yet"

  eval ContinueExpr = throwError $ InternalError "not implemented yet"

  eval (ReturnExpr e) = throwError $ InternalError "not implemented yet"
