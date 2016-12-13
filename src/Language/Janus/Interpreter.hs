module Language.Janus.Interpreter (
  Evaluable,
  eval,
  EvalException(..)
) where

import           Control.Exception  (Exception)

import           Language.Janus.AST


data EvalException = TypeException
                   | InternalError String
                   deriving (Eq, Ord)

instance Show EvalException where
  show TypeException       = "type mismatch"
  show (InternalError msg) = "internal error: " ++ msg

instance Exception EvalException


class Evaluable a where
  eval :: a -> Either EvalException Val

instance Evaluable Val where
  eval = Right

instance Evaluable Expr where
  eval (LiteralExpr a) = eval a

  eval (BlockExpr block) = Left $ InternalError "not implemented yet"

  eval (ParenExpr e) = eval e

  eval (IndexExpr e idx) = Left $ InternalError "not implemented yet"

  eval (CallExpr e args) = Left $ InternalError "not implemented yet"

  eval (PostfixIncExpr e) = Left $ InternalError "not implemented yet"

  eval (PostfixDecExpr e) = Left $ InternalError "not implemented yet"

  eval (NotExpr e) = Left $ InternalError "not implemented yet"

  eval (BitNotExpr e) = Left $ InternalError "not implemented yet"

  eval (PlusExpr e) = Left $ InternalError "not implemented yet"

  eval (NegExpr e) = Left $ InternalError "not implemented yet"

  eval (PrefixIncExpr e) = Left $ InternalError "not implemented yet"

  eval (PrefixDecExpr e) = Left $ InternalError "not implemented yet"

  eval (ExpExpr a b) = Left $ InternalError "not implemented yet"

  eval (MulExpr a b) = Left $ InternalError "not implemented yet"

  eval (DivExpr a b) = Left $ InternalError "not implemented yet"

  eval (RemExpr a b) = Left $ InternalError "not implemented yet"

  eval (AddExpr a b) = Left $ InternalError "not implemented yet"

  eval (SubExpr a b) = Left $ InternalError "not implemented yet"

  eval (LshExpr a b) = Left $ InternalError "not implemented yet"

  eval (RshExpr a b) = Left $ InternalError "not implemented yet"

  eval (BitAndExpr a b) = Left $ InternalError "not implemented yet"

  eval (BitXorExpr a b) = Left $ InternalError "not implemented yet"

  eval (BitOrExpr a b) = Left $ InternalError "not implemented yet"

  eval (EqExpr a b) = Left $ InternalError "not implemented yet"

  eval (NeqExpr a b) = Left $ InternalError "not implemented yet"

  eval (LtExpr a b) = Left $ InternalError "not implemented yet"

  eval (GtExpr a b) = Left $ InternalError "not implemented yet"

  eval (LtEqExpr a b) = Left $ InternalError "not implemented yet"

  eval (GtEqExpr a b) = Left $ InternalError "not implemented yet"

  eval (AndExpr a b) = Left $ InternalError "not implemented yet"

  eval (OrExpr a b) = Left $ InternalError "not implemented yet"

  eval IfExpr{ifCond=cond, ifBranch=a, elseBranch=b} =
    Left $ InternalError "not implemented yet"

  eval WhileExpr{whileCond=cond, whileBody=body} =
    Left $ InternalError "not implemented yet"

  eval (LoopExpr e) = Left $ InternalError "not implemented yet"

  eval BreakExpr = Left $ InternalError "not implemented yet"

  eval ContinueExpr = Left $ InternalError "not implemented yet"

  eval (ReturnExpr e) = Left $ InternalError "not implemented yet"
