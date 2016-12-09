module Language.Janus.Interpreter.Evaluable where

import           Control.Exception  (Exception)

import           Language.Janus.AST (Val (..))


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
