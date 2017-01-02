module Language.Janus.Interp where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

data EvalState

data EvalError

type InterpM = StateT EvalState (ExceptT EvalError IO)
