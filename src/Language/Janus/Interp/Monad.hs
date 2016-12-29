{-# LANGUAGE DuplicateRecordFields #-}

module Language.Janus.Interp.Monad (
  ObjPtr,
  maxObjCount,

  EvalState,
  emptyState,

  InterpM,
  iie,

  pushScope,
  popFrame,

  allSymbols
) where

import           Control.Monad               (foldM)
import           Control.Monad.Except        (ExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, get, gets, modify, put)

import qualified Data.HashTable.IO           as HM
import qualified Data.Set                    as S

import           Language.Janus.AST.Val
import           Language.Janus.Interp.Error


type MHashTable k v = HM.BasicHashTable k v


--
-- Object Pointer
--
type ObjPtr = Word

maxObjCount :: Integer
maxObjCount = toInteger (maxBound :: ObjPtr)


--
-- Memory Cell
--
data MemCell = MemCell {
                mptr     :: ObjPtr,
                refcount :: Word,
                val      :: Val
              }
             deriving (Eq, Show)


--
-- StackFrame
--
data StackFrame = ScopeFrame {
                    symbols     :: MHashTable String ObjPtr
                  }
                | CallFrame {
                    -- TODO Implement this properly
                    func        :: ObjPtr
                  }

newScopeFrame :: MonadIO m => m StackFrame
newScopeFrame = do
  symbols <- liftIO HM.new
  return ScopeFrame { symbols = symbols }

-- TODO newCallFrame


--
-- EvalState
--
data EvalState = EvalState {
                  nextMptr :: ObjPtr,
                  mem      :: MHashTable ObjPtr MemCell,
                  stack    :: [StackFrame]
                }

emptyState :: MonadIO m => m EvalState
emptyState = do
  mem <- liftIO HM.new
  globalScope <- newScopeFrame
  return EvalState {
      nextMptr = 0,
      mem = mem,
      stack = [globalScope]
    }


--
-- InterpM
--
type InterpM = StateT EvalState (ExceptT EvalError IO)

iie :: String -> InterpM a
iie = throwError . InternalError


--
-- State methods: Stack manipulation
--
rawPushFrame :: StackFrame -> InterpM ()
rawPushFrame sf = modify (\st -> st { stack = sf : stack st })

pushScope :: InterpM ()
pushScope = newScopeFrame >>= rawPushFrame

rawPopFrame :: InterpM StackFrame
rawPopFrame = do
  st <- get
  case stack st of
    [_] -> iie "stack underflow"
    (top:fs) -> do
      put st { stack = fs }
      return top

popFrame :: InterpM ()
popFrame = do
  frame <- rawPopFrame
  freeFrame frame
  where
    freeFrame :: StackFrame -> InterpM ()
    freeFrame ScopeFrame{symbols=symbols} = iie "freeing scopes is not implemented yet"
    freeFrame _                           = return ()


--
-- State methods: Symbol manipulation
--
allSymbols :: InterpM [String]
allSymbols = do
  stack <- gets stack
  symbolSet <- liftIO
    . foldM aggfn S.empty
    $ stack
  return $ S.toList symbolSet
  where
    aggfn :: S.Set String -> StackFrame -> IO (S.Set String)
    aggfn set ScopeFrame{symbols=symbols} = do
      names <- getNames symbols
      return $ names `S.union` set
    aggfn set _                           = return set

    getNames :: MHashTable String ObjPtr -> IO (S.Set String)
    getNames syms = do
      kvs <- HM.toList syms
      let keys = fmap fst kvs
      return $ S.fromList keys
