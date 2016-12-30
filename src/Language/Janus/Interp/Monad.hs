{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Language.Janus.Interp.Monad (
  ObjPtr,
  maxObjCount,

  EvalState,
  emptyState,

  InterpM,
  iie,

  pushScope,
  popFrame,

  memIsFree,
  memGetVal,
  memGetRc,
  memAlloc,
  memSet,
  rcIncr,
  rcDecr,

  lookupSymbol,
  putSymbol,
  evalSymbol,
  allSymbols
) where

import           Control.Monad
import           Control.Monad.Except        (ExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, get, gets, modify, put)
import           Data.Maybe                  (isNothing)

import qualified Data.HashTable.IO           as HM
import qualified Data.Set                    as S

import           Language.Janus.AST.Val
import           Language.Janus.Interp.Error


-- TODO Find out which implementation is the fastest
type MHashTable k v = HM.CuckooHashTable k v


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
  case frame of
    ScopeFrame{symbols=syms} -> liftIO (HM.toList syms) >>= mapM_ (rcDecr . snd)
    _ -> return ()


--
-- State methods: References
--
memIsFree :: ObjPtr -> InterpM Bool
memIsFree ptr = do { mem <- gets mem; isNothing <$> liftIO (HM.lookup mem ptr) }

memGetVal :: ObjPtr -> InterpM Val
memGetVal ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  return $ val cell

memGetRc :: ObjPtr -> InterpM Word
memGetRc ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  return $ refcount cell

-- malloc in Haskell XD
memAlloc :: Val -> InterpM ObjPtr
memAlloc val = do
  mem <- gets mem
  ptr <- gets nextMptr
  when (ptr == maxBound) $ throwError OutOfMemory
  modify $ \st -> st { nextMptr = nextMptr st + 1 }
  liftIO $ HM.insert mem ptr MemCell { refcount = 0, val = val }
  return ptr

memSet :: ObjPtr -> Val -> InterpM ()
memSet ptr val = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { val = val }

rcIncr :: ObjPtr -> InterpM ()
rcIncr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { refcount = refcount cell + 1 }

rcDecr :: ObjPtr -> InterpM ()
rcDecr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  case refcount cell - 1 of
      0  -> liftIO $ HM.delete mem ptr
      rc -> liftIO $ HM.insert mem ptr cell { refcount = rc }


--
-- State methods: Symbol manipulation
--
lookupSymbol :: String -> InterpM ObjPtr
lookupSymbol name = gets stack >>= doLookup Nothing
  where
    doLookup :: Maybe ObjPtr -> [StackFrame] -> InterpM ObjPtr
    doLookup (Just ptr) _ = return ptr
    doLookup _ []         = throwError $ UndefinedSymbol name
    doLookup _ (ScopeFrame{symbols=syms}:frs) = do
      l <- liftIO $ HM.lookup syms name
      doLookup l frs
    doLookup _ (_:frs) = doLookup Nothing frs

putSymbol :: String -> ObjPtr -> InterpM ()
putSymbol name ptr = do
  syms <- gets $ symbols . head . stack

  rcIncr ptr

  -- decrement existing reference if any
  existingPtr' <- liftIO $ HM.lookup syms name
  case existingPtr' of
    Just existingPtr -> rcDecr existingPtr
    _                -> return ()

  liftIO $ HM.insert syms name ptr

evalSymbol :: String -> InterpM Val
evalSymbol name = lookupSymbol name >>= memGetVal

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
      let keys = map fst kvs
      return $ S.fromList keys


--
-- Misc
--
throwIfNothing :: InterpM (Maybe a) -> EvalError -> InterpM a
throwIfNothing valM err = do
  val' <- valM
  case val' of
    Nothing  -> throwError err
    Just val -> return val
