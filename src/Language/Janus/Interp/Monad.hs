module Language.Janus.Interp.Monad (
  EvalState(..),
  emptyState,

  InterpM
) where

import           Control.Monad.Except        (ExceptT)
import           Control.Monad.State.Strict  (StateT)

import           Language.Janus.Interp.Error

data EvalState = EvalState
               deriving (Eq, Show)

emptyState :: EvalState
emptyState = EvalState

type InterpM = StateT EvalState (ExceptT EvalError IO)

