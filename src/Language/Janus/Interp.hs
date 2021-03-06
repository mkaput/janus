{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Janus.Interp (
  EvalState,

  EvalError (
    OpCallTypeError,
    ItemCallError,
    IndexOutOfBounds,
    InternalError,
    InvalidPointer,
    ExpectedBool,
    ExpectedRef,
    NotCallable,
    OutOfMemory,
    UndefinedSymbol,

    CustomError
  ),

  InterpM,
  runInterpM,
  run,

  deref,
  refset,

  pushScope,
  popFrame,

  memIsFree,
  memGetVal,
  memGetRc,
  malloc,
  memset,
  rcIncr,
  rcDecr,

  lookupVar,
  putVar,
  evalVal,
  allVars,

  Evaluable(eval),
  RefEvaluable(evalRef, tryEvalRef),
  Callable(call),

  valGetIdx,
  valSetIdx,
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.Bits                  (complement, rotateL, rotateR, xor,
                                             (.&.), (.|.))
import           Data.List                  (foldl')
import           Data.Maybe                 (isNothing, listToMaybe)
import           Data.Typeable              (TypeRep, Typeable, typeOf)
import           Text.Printf                (printf)

import qualified Data.HashTable.IO          as HM
import qualified Data.Set                   as S

import           Language.Janus.AST


-----------------------------------------------------------------------------
--
-- Memory Cell
--
-----------------------------------------------------------------------------

data MemCell = MemCell {
                refcount :: Word,
                val      :: Val
              }
             deriving (Eq, Show)


-----------------------------------------------------------------------------
--
-- StackFrame
--
-----------------------------------------------------------------------------

data StackFrame = ScopeFrame {
                    symbols :: MHashTable String Ptr
                  }
                | CallFrame {
                    item :: Item
                  }
                | BlockFrame
                | LoopFrame

newScopeFrame :: MonadIO m => m StackFrame
newScopeFrame = do
  symbols <- liftIO HM.new
  return ScopeFrame { symbols = symbols }


-----------------------------------------------------------------------------
--
-- EvalState
--
-----------------------------------------------------------------------------

-- |
-- Intepreter state.
data EvalState = EvalState {
                  nextMptr :: Ptr,
                  mem      :: MHashTable Ptr MemCell,
                  stack    :: [StackFrame]
                }

emptyState :: MonadIO m => m EvalState
emptyState = do
  mem <- liftIO HM.new
  globalScope <- newScopeFrame
  return EvalState {
      nextMptr = Ptr 0,
      mem = mem,
      stack = [globalScope]
    }


-----------------------------------------------------------------------------
--
-- Interpreter error
--
-----------------------------------------------------------------------------

-- |
-- Interpreter error.
data EvalError = OpCallTypeError {
                  opName    :: String,
                  triedSigs :: [[TypeRep]],
                  givenSig  :: [TypeRep]
                }                             -- ^ Operator invocation type error
               | ItemCallError Item EvalError -- ^ 'Callable' (e.g. function) call error
               | IndexOutOfBounds             -- ^ Tried to access out of bounds index
               | InternalError String         -- ^ Internal interpreter error - a bug
               | InvalidPointer Ptr           -- ^ Tried to access unallocated memory cell
               | ExpectedBool Val             -- ^ Expected boolean value (e.g. in condition)
               | ExpectedRef                  -- ^ Expected reference (e.g. in call operator)
               | NotCallable Val              -- ^ Given value is not callable
               | OutOfMemory                  -- ^ Cannot allocate more memory cells
               | UndefinedSymbol String       -- ^ Cannot resolve requested symbol

               | CustomError String           -- ^ In-code exception

               | LoopBreak_
               | LoopContinue_
               | FuncReturn_ Val
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

  show (ItemCallError item cause) = "error calling " ++ show item ++ ": \n" ++ showIndented cause

  show IndexOutOfBounds = "index out of bounds"

  show (InternalError msg) = "Internal error: " ++ msg

  show (InvalidPointer ptr) = printf "Invalid pointer: 0x%08x" (getAddress ptr)

  show (ExpectedBool val) = "expected boolean value, got " ++ showVal val

  show ExpectedRef = "expected reference expression"

  show (NotCallable val) = showVal val ++ " is not callable"

  show OutOfMemory = "out of memory"

  show (UndefinedSymbol name) = "undefined symbol " ++ name

  show (CustomError msg) = msg


-----------------------------------------------------------------------------
--
-- InterpM
--
-----------------------------------------------------------------------------

-- |
-- Interpreter monad.
type InterpM = StateT EvalState (ExceptT EvalError IO)

-- |
-- Run action inside clean `InterpM'.
runInterpM :: InterpM a -> IO (Either EvalError a)
runInterpM m = do { st <- emptyState; runExceptT (evalStateT m st) }

-- |
-- Evaluate 'Evaluable' entity inside clean 'InterpM'.
run :: Evaluable a => a -> IO (Either EvalError Val)
run = runInterpM . eval


-----------------------------------------------------------------------------
--
-- Ref methods
--
-----------------------------------------------------------------------------

-- |
-- Dereference a 'Ref' - get 'Val' given reference points to.
deref :: Ref -> InterpM Val
deref (PtrRef ptr)       = memGetVal ptr
deref (IndexRef ptr idx) = memGetVal ptr >>= (`valGetIdx` idx)

-- |
-- Alter value given 'Ref' points to.
refset :: Ref -> Val -> InterpM ()
refset (PtrRef ptr) newVal       = memset ptr newVal
refset (IndexRef ptr idx) newVal = do
  a <- memGetVal ptr
  newVal <- valSetIdx a idx newVal
  memset ptr newVal


-----------------------------------------------------------------------------
--
-- State methods: Stack manipulation
--
-----------------------------------------------------------------------------

rawPushFrame :: StackFrame -> InterpM ()
rawPushFrame sf = modify' (\st -> st { stack = sf : stack st })

rawPopFrame :: InterpM StackFrame
rawPopFrame = do
  st <- get
  case stack st of
    [_] -> iie "stack underflow"
    (top:fs) -> do
      put st { stack = fs }
      return top

-- |
-- Pop top stack frame.
popFrame :: InterpM ()
popFrame = do
  frame <- rawPopFrame
  case frame of
    ScopeFrame{symbols=syms} -> liftIO (HM.toList syms) >>= mapM_ (rcDecr . snd)
    _ -> return ()

-- |
-- Push new scope.
pushScope :: InterpM ()
pushScope = newScopeFrame >>= rawPushFrame

pushCallFrame :: Item -> InterpM ()
pushCallFrame = rawPushFrame . CallFrame

pushBlockFrame :: InterpM ()
pushBlockFrame = rawPushFrame BlockFrame

pushLoopFrame :: InterpM ()
pushLoopFrame = rawPushFrame LoopFrame

cleanLoopFrame :: InterpM ()
cleanLoopFrame = rawPopFrame >>= doClean
  where
    doClean LoopFrame      = return ()
    doClean ScopeFrame{..} = rawPopFrame >>= doClean
    doClean _              = iie "dirty stack"

cleanBlockFrame :: InterpM ()
cleanBlockFrame = rawPopFrame >>= doClean
  where
    doClean BlockFrame     = return ()
    doClean ScopeFrame{..} = rawPopFrame >>= doClean
    doClean _              = iie "dirty stack"


-----------------------------------------------------------------------------
--
-- State methods: References
--
-----------------------------------------------------------------------------

-- |
-- Check whether given pointer points to unallocated memory cell.
memIsFree :: Ptr -> InterpM Bool
memIsFree ptr = do { mem <- gets mem; isNothing <$> liftIO (HM.lookup mem ptr) }

-- |
-- Get value of given memory cell.
memGetVal :: Ptr -> InterpM Val
memGetVal ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `unwrapM'` InvalidPointer ptr
  return $ val cell

-- |
-- Get reference count of given memory cell.
memGetRc :: Ptr -> InterpM Word
memGetRc ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `unwrapM'` InvalidPointer ptr
  return $ refcount cell

-- malloc in Haskell XD
-- |
-- Allocate value in interpreter memory. Returns new memory cell's address.
malloc :: Val -> InterpM Ptr
malloc val = do
  mem <- gets mem
  ptr <- gets nextMptr
  when (ptr == maxBound) $ throwError OutOfMemory
  modify' $ \st -> st { nextMptr = Ptr $ getAddress ptr + 1 }
  liftIO $ HM.insert mem ptr MemCell { refcount = 0, val = val }
  return ptr

-- |
-- Set value at given address.
memset :: Ptr -> Val -> InterpM ()
memset ptr val = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `unwrapM'` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { val = val }

-- |
-- Increment reference count of memory cell at given address.
rcIncr :: Ptr -> InterpM ()
rcIncr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `unwrapM'` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { refcount = refcount cell + 1 }

-- |
-- Decrement reference count of memory cell at given address.
-- If the RC will reach 0, the cell will be deallocated.
rcDecr :: Ptr -> InterpM ()
rcDecr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `unwrapM'` InvalidPointer ptr
  case refcount cell - 1 of
      0  -> liftIO $ HM.delete mem ptr
      rc -> liftIO $ HM.insert mem ptr cell { refcount = rc }


-----------------------------------------------------------------------------
--
-- State methods: Symbol manipulation
--
-----------------------------------------------------------------------------

-- |
-- Lookup variable in all available scopes.
lookupVar :: String -> InterpM Ptr
lookupVar name = gets stack >>= doLookup Nothing
  where
    doLookup :: Maybe Ptr -> [StackFrame] -> InterpM Ptr
    doLookup (Just ptr) _ = return ptr
    doLookup _ []         = throwError $ UndefinedSymbol name
    doLookup _ (ScopeFrame{symbols=syms}:frs) = do
      l <- liftIO $ HM.lookup syms name
      doLookup l frs
    doLookup _ (_:frs) = doLookup Nothing frs

-- |
-- Create new, or change existing, variable binding.
-- If the variable has been already exising, its old value's
-- reference count will be decremented.
putVar :: String -> Ptr -> InterpM ()
putVar name ptr = do
  syms <- gets $ symbols . head . filterScopes . stack

  rcIncr ptr

  -- decrement existing reference if any
  existingPtr' <- liftIO $ HM.lookup syms name
  case existingPtr' of
    Just existingPtr -> rcDecr existingPtr
    _                -> return ()

  liftIO $ HM.insert syms name ptr
  where
    filterScopes = filter $ \frm -> case frm of
      ScopeFrame{..} -> True
      _              -> False

-- |
-- @
--    evalVal name = lookupVar name >>= memGetVal
-- @
evalVal :: String -> InterpM Val
evalVal name = lookupVar name >>= memGetVal

-- |
-- Enumerate all visible symbols.
allVars :: InterpM [String]
allVars = do
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

    getNames :: MHashTable String Ptr -> IO (S.Set String)
    getNames syms = do
      kvs <- HM.toList syms
      let keys = map fst kvs
      return $ S.fromList keys


-----------------------------------------------------------------------------
--
-- Evaluable
--
-----------------------------------------------------------------------------

-- |
-- The Evaluable class denotes enitity which can be evaluated to 'Val'.
class Evaluable a where
  eval :: a -> InterpM Val

instance Evaluable Val where
  eval = return

instance Evaluable EvalError where
  eval = throwError

instance Evaluable Program where
  eval (Program stmts) = eval (Block stmts)

instance Evaluable Lvalue where
  eval lv = evalRef lv >>= deref

instance Evaluable Expr where
  eval (LiteralExpr a') = eval a'

  eval (BlockExpr block') = eval block'

  eval (ParenExpr e') = eval e'

  eval (CallExpr e' args') = do
    item <- eval e'
    args <- mapM eval args'
    item `call` args

  eval (PostfixIncExpr lv) = do
    ref <- evalRef lv
    val <- eval lv
    newVal <- valIncr "a++" val
    refset ref newVal
    return val

  eval (PostfixDecExpr lv) = do
    ref <- evalRef lv
    val <- eval lv
    newVal <- valDecr "a--" val
    refset ref newVal
    return val

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

  eval (PrefixIncExpr lv) = do
    ref <- evalRef lv
    newVal <- eval lv >>= valIncr "++a"
    refset ref newVal
    return newVal

  eval (PrefixDecExpr lv) = do
    ref <- evalRef lv
    newVal <- eval lv >>= valDecr "--a"
    refset ref newVal
    return newVal

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

  eval IfExpr{cond=c', ifBranch=a', elseBranch=b'} = do
    c <- evalBool c'
    if c then
      eval a'
    else
      maybe (return JUnit) eval b'

  eval WhileExpr{cond=c', body=b'} = doLoop $ do
    c <- evalBool c'
    unless c $ throwError LoopBreak_
    eval b'

  eval (LoopExpr e') = doLoop (eval e')

  eval BreakExpr = throwError LoopBreak_

  eval ContinueExpr = throwError LoopContinue_

  eval (ReturnExpr e') = eval e' >>= throwError . FuncReturn_

  eval (LvalueExpr lv) = eval lv

instance Evaluable Block where
  eval (Block stmts) = do
    pushBlockFrame
    result <- foldM (\_ stmt -> eval stmt) JUnit stmts
    cleanBlockFrame
    return result

instance Evaluable Stmt where
  eval (LetDecl name e') = do
    pushScope
    ptr <- meanPtr e'
    putVar name ptr
    return JUnit
    where meanPtr e' = tryEvalRef e' >>= \x -> case x of
                         Just (PtrRef ptr) -> return ptr
                         _                 -> eval e' >>= malloc

  eval (FnDecl n p b) = do
    malloc (JItem $ Func n p b) >>= putVar n
    return JUnit

  eval (SubstStmt lv' e') = do
    ref <- evalRef lv'
    val <- eval e'
    refset ref val
    return JUnit

  eval (ExprStmt e')      = eval e'


-----------------------------------------------------------------------------
--
-- RefEvaluable
--
-----------------------------------------------------------------------------

-- |
-- The RefEvaluable class denotes enitity which can be evaluated to 'Ref'
class RefEvaluable a where
  -- |
  -- Throws 'ExpectedRef' if 'tryEvalRef' returns 'Nothing'.
  evalRef :: a -> InterpM Ref
  evalRef a = tryEvalRef a `unwrapM'` ExpectedRef

  tryEvalRef :: a -> InterpM (Maybe Ref)
  tryEvalRef a = Just `fmap` evalRef a

  {-# MINIMAL evalRef | tryEvalRef #-}

instance RefEvaluable Ptr where
  evalRef = return . PtrRef

instance RefEvaluable Ref where
  evalRef = return

instance RefEvaluable Lvalue where
  evalRef (Path name) = lookupVar name >>= evalRef
  evalRef (IndexLv name idx') = do
    ptr <- lookupVar name
    idx <- eval idx'
    return $ IndexRef ptr idx

instance RefEvaluable Expr where
  tryEvalRef (ParenExpr e')  = tryEvalRef e'

  tryEvalRef (LvalueExpr lv) = tryEvalRef lv

  tryEvalRef _               = return Nothing


-----------------------------------------------------------------------------
--
-- Callable
--
-----------------------------------------------------------------------------

-- |
-- The Callable class denotes enitity which can be called, such as functions.
class Callable a where
  call :: a -> [Val] -> InterpM Val

instance Callable Val where
  call (JItem item) args = item `call` args

  call v _               = throwError $ NotCallable v

instance Callable Item where
  call item@(Func n p b') args = do
    pushCallFrame item
    pushScope

    let vars = zip p (args ++ repeat JUnit)
    forM_ vars $ \(vname, vval) -> malloc vval >>= putVar vname

    result <- eval b' `catchError` \err -> case err of
      FuncReturn_ result -> return result
      err                -> throwError $ ItemCallError item err

    popFrame
    popFrame

    return result

  call item@(NativeFunc _ _ f) args = f args `catchError` (throwError . ItemCallError item)


-----------------------------------------------------------------------------
--
-- Index accessors
--
-----------------------------------------------------------------------------

valGetIdx :: Val         -- ^ Container value
          -> Val         -- ^ Index
          -> InterpM Val -- ^ Value inside container at given index
valGetIdx (JStr s) (JInt n)
  | n < 0 = throwError IndexOutOfBounds
  | otherwise = (
        listToMaybe . map (JChar . snd) . filter ((== n) . fst) $ zip [0..] s
      ) `unwrapM` IndexOutOfBounds
valGetIdx v n = throwError OpCallTypeError {
    opName = "a[i]",
    triedSigs = [[typeOf "", typeOf (undefined :: Integer), typeOf 'a']],
    givenSig = [haskellTypeRep v, haskellTypeRep n]
  }

valSetIdx :: Val         -- ^ Container value
          -> Val         -- ^ Index
          -> Val         -- ^ New Value
          -> InterpM Val -- ^ New container with value at given index changed
valSetIdx (JStr s) (JInt n) (JChar c)
  | n < 0 = throwError IndexOutOfBounds
  | otherwise = do
    when (fromInteger n >= length s) $ throwError IndexOutOfBounds
    return . JStr . map (\(i, ch) -> if i == n then c else ch) $ zip [0..] s
valSetIdx v n x = throwError OpCallTypeError {
    opName = "a[i] = x",
    triedSigs = [[typeOf "", typeOf (undefined :: Integer), typeOf 'a', typeOf ()]],
    givenSig = [haskellTypeRep v, haskellTypeRep n, haskellTypeRep x]
  }


-----------------------------------------------------------------------------
--
-- Function wrapping
--
-----------------------------------------------------------------------------

wrapOp1 :: forall a b. (FromVal a, ToVal b, Typeable a, Typeable b)
        => (a -> b)
        -> (Val -> Maybe Val, [TypeRep])
wrapOp1 f = (
    fmap (toVal . f) . tryFromVal,
    [typeOf (undefined :: a), typeOf (undefined :: b)]
  )

callOp1 :: Evaluable a
        => String
        -> [(Val -> Maybe Val, [TypeRep])]
        -> a
        -> InterpM Val
callOp1 opName fs a' = do
  a <- eval a'
  doCall fs a []
    where
      doCall :: [(Val -> Maybe Val, [TypeRep])]
             -> Val
             -> [[TypeRep]]
             -> InterpM Val
      doCall [] a triedSigs = throwError OpCallTypeError {
          opName = opName,
          triedSigs = triedSigs,
          givenSig = [haskellTypeRep a]
        }
      doCall ((f, sig):fs) a triedSigs = case f a of
        Just v  -> return v
        Nothing -> doCall fs a (sig:triedSigs)

wrapOp2 :: forall a b c. (FromVal a, FromVal b, ToVal c,
                          Typeable a, Typeable b, Typeable c)
        => (a -> b -> c)
        -> (Val -> Val -> Maybe Val, [TypeRep])
wrapOp2 f = (
    \a' b' -> do
      a <- tryFromVal a'
      b <- tryFromVal b'
      return . toVal $ f a b,
    [typeOf (undefined :: a), typeOf (undefined :: b), typeOf (undefined :: c)]
  )

callOp2 :: (Evaluable a, Evaluable b)
        => String
        -> [(Val -> Val -> Maybe Val, [TypeRep])]
        -> a
        -> b
        -> InterpM Val
callOp2 opName fs a' b' = do
  a <- eval a'
  b <- eval b'
  doCall fs a b []
    where
      doCall :: [(Val -> Val -> Maybe Val, [TypeRep])]
             -> Val
             -> Val
             -> [[TypeRep]]
             -> InterpM Val
      doCall [] a b triedSigs = throwError OpCallTypeError {
          opName = opName,
          triedSigs = triedSigs,
          givenSig = [haskellTypeRep a, haskellTypeRep b]
        }
      doCall ((f, sig):fs) a b triedSigs = case f a b of
        Just v  -> return v
        Nothing -> doCall fs a b (sig:triedSigs)


-----------------------------------------------------------------------------
--
-- Utility funcitons
--
-----------------------------------------------------------------------------

-- TODO Find out which implementation is the fastest
type MHashTable k v = HM.CuckooHashTable k v

iie :: String -> InterpM a
iie = throwError . InternalError

unwrapM :: Maybe a -> EvalError -> InterpM a
unwrapM val err = return val `unwrapM'` err

unwrapM' :: InterpM (Maybe a) -> EvalError -> InterpM a
unwrapM' valM err = do
  val' <- valM
  case val' of
    Nothing  -> throwError err
    Just val -> return val

evalBool :: Evaluable a => a -> InterpM Bool
evalBool e' = do { ev <- eval e'; tryFromVal ev `unwrapM` ExpectedBool ev }

doLoop :: InterpM Val -> InterpM Val
doLoop f = (forever loopBody >> return JUnit) `catchError` handleLoopBreak
  where
    loopBody = do { pushLoopFrame; f; cleanLoopFrame } `catchError` handleLoopContinue

    handleLoopContinue :: EvalError -> InterpM ()
    handleLoopContinue LoopContinue_ = return ()
    handleLoopContinue ex            = throwError ex

    handleLoopBreak :: EvalError -> InterpM Val
    handleLoopBreak LoopBreak_ = return JUnit
    handleLoopBreak ex         = throwError ex

valIncr :: String -> Val -> InterpM Val
valIncr opName = callOp1 opName [
    wrapOp1 ((+ 1) :: Integer -> Integer),
    wrapOp1 ((+ 1.0) :: Double -> Double)
  ]

valDecr :: String -> Val -> InterpM Val
valDecr opName = callOp1 opName [
    wrapOp1 ((\x -> x - 1) :: Integer -> Integer),
    wrapOp1 ((\x -> x - 1.0) :: Double -> Double)
  ]

showIndented :: Show a => a -> String
showIndented = unlines . map ("  " ++) . lines . show
