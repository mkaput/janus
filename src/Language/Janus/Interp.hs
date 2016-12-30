{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Janus.Interp (
  EvalState,
  emptyState,

  EvalError(..),

  InterpM,
  runInterpM,
  run,

  deref,
  refSet,

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
  allSymbols,

  Evaluable,
  eval,

  valGetIdx,
  valSetIdx
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
                    symbols     :: MHashTable String Ptr
                  }
                | CallFrame {
                    -- TODO Implement this properly
                    func        :: Ptr
                  }

newScopeFrame :: MonadIO m => m StackFrame
newScopeFrame = do
  symbols <- liftIO HM.new
  return ScopeFrame { symbols = symbols }

-- TODO newCallFrame


-----------------------------------------------------------------------------
--
-- EvalState
--
-----------------------------------------------------------------------------

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

data EvalError = OpCallTypeError {
                  opName    :: String,
                  triedSigs :: [[TypeRep]],
                  givenSig  :: [TypeRep]
                }
               | IndexOutOfBounds
               | InternalError String
               | InvalidPointer Ptr
               | OutOfMemory
               | UndefinedSymbol String
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

  show IndexOutOfBounds = "index out of bounds"

  show (InternalError msg) = "Internal error: " ++ msg

  show (InvalidPointer ptr) = printf "Invalid pointer: 0x%08x" (getAddress ptr)

  show OutOfMemory = "out of memory"

  show (UndefinedSymbol name) = "undefined symbol " ++ name


-----------------------------------------------------------------------------
--
-- InterpM
--
-----------------------------------------------------------------------------

type InterpM = StateT EvalState (ExceptT EvalError IO)

runInterpM :: InterpM a -> IO (Either EvalError a)
runInterpM m = do { st <- emptyState; runExceptT (evalStateT m st) }

run :: Evaluable a => a -> IO (Either EvalError Val)
run = runInterpM . eval


-----------------------------------------------------------------------------
--
-- Ref methods
--
-----------------------------------------------------------------------------

deref :: Ref -> InterpM Val
deref (PtrRef ptr)       = memGetVal ptr
deref (IndexRef ptr idx) = memGetVal ptr >>= (`valGetIdx` idx)

refSet :: Ref -> Val -> InterpM ()
refSet (PtrRef ptr) newVal       = memSet ptr newVal
refSet (IndexRef ptr idx) newVal = do
  currObj <- memGetVal ptr
  newObj <- valSetIdx currObj idx newVal
  memSet ptr newObj


-----------------------------------------------------------------------------
--
-- State methods: Stack manipulation
--
-----------------------------------------------------------------------------

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


-----------------------------------------------------------------------------
--
-- State methods: References
--
-----------------------------------------------------------------------------

memIsFree :: Ptr -> InterpM Bool
memIsFree ptr = do { mem <- gets mem; isNothing <$> liftIO (HM.lookup mem ptr) }

memGetVal :: Ptr -> InterpM Val
memGetVal ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  return $ val cell

memGetRc :: Ptr -> InterpM Word
memGetRc ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  return $ refcount cell

-- malloc in Haskell XD
memAlloc :: Val -> InterpM Ptr
memAlloc val = do
  mem <- gets mem
  ptr <- gets nextMptr
  when (ptr == maxBound) $ throwError OutOfMemory
  modify $ \st -> st { nextMptr = Ptr $ getAddress ptr + 1 }
  liftIO $ HM.insert mem ptr MemCell { refcount = 0, val = val }
  return ptr

memSet :: Ptr -> Val -> InterpM ()
memSet ptr val = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { val = val }

rcIncr :: Ptr -> InterpM ()
rcIncr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  liftIO $ HM.insert mem ptr cell { refcount = refcount cell + 1 }

rcDecr :: Ptr -> InterpM ()
rcDecr ptr = do
  mem <- gets mem
  cell <- liftIO (HM.lookup mem ptr) `throwIfNothing` InvalidPointer ptr
  case refcount cell - 1 of
      0  -> liftIO $ HM.delete mem ptr
      rc -> liftIO $ HM.insert mem ptr cell { refcount = rc }


-----------------------------------------------------------------------------
--
-- State methods: Symbol manipulation
--
-----------------------------------------------------------------------------

lookupSymbol :: String -> InterpM Ptr
lookupSymbol name = gets stack >>= doLookup Nothing
  where
    doLookup :: Maybe Ptr -> [StackFrame] -> InterpM Ptr
    doLookup (Just ptr) _ = return ptr
    doLookup _ []         = throwError $ UndefinedSymbol name
    doLookup _ (ScopeFrame{symbols=syms}:frs) = do
      l <- liftIO $ HM.lookup syms name
      doLookup l frs
    doLookup _ (_:frs) = doLookup Nothing frs

putSymbol :: String -> Ptr -> InterpM ()
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

class Evaluable a where
  eval :: a -> InterpM Val

instance Evaluable Val where
  eval = return

instance Evaluable EvalError where
  eval = throwError

instance Evaluable Lvalue where
  eval (IndexLv v' idx') = iie "not implemented yet"

  eval (Path name)       = evalSymbol name

instance Evaluable Expr where
  eval (LiteralExpr a') = eval a'

  eval (BlockExpr block') = iie "not implemented yet"

  eval (ParenExpr e') = eval e'

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

  eval IfExpr{cond=cond', ifBranch=a', elseBranch=b'} = iie "not implemented yet"

  eval WhileExpr{cond=cond', body=body'} = iie "not implemented yet"

  eval (LoopExpr e') = iie "not implemented yet"

  eval BreakExpr = iie "not implemented yet"

  eval ContinueExpr = iie "not implemented yet"

  eval (ReturnExpr e') = iie "not implemented yet"

  eval (LvalueExpr lv) = eval lv


-----------------------------------------------------------------------------
--
-- Val index accessors
--
-----------------------------------------------------------------------------

valGetIdx :: Val -> Val -> InterpM Val
valGetIdx (JStr s) (JInt n)
  | n < 0 = throwError IndexOutOfBounds
  | otherwise = return (
        listToMaybe . map (JChar . snd) . filter ((== n) . fst) $ zip [0..] s
      ) `throwIfNothing` IndexOutOfBounds
valGetIdx v n = throwError OpCallTypeError {
    opName = "a[i]",
    triedSigs = [[typeOf "", typeOf (undefined :: Integer)]],
    givenSig = [haskellTypeRep v, haskellTypeRep n]
  }

valSetIdx :: Val -> Val -> Val -> InterpM Val
valSetIdx (JStr s) (JInt n) (JChar c)
  | n < 0 = throwError IndexOutOfBounds
  | otherwise = do
    when (fromInteger n >= length s) $ throwError IndexOutOfBounds
    return . JStr . map (\(i, ch) -> if i == n then c else ch) $ zip [0..] s
valSetIdx v n x = throwError OpCallTypeError {
    opName = "a[i] = x",
    triedSigs = [[typeOf "", typeOf (undefined :: Integer), typeOf 'a']],
    givenSig = [haskellTypeRep v, haskellTypeRep n, haskellTypeRep x]
  }


-----------------------------------------------------------------------------
--
-- Utility funcitons
--
-----------------------------------------------------------------------------

-- TODO Find out which implementation is the fastest
type MHashTable k v = HM.CuckooHashTable k v

iie :: String -> InterpM a
iie = throwError . InternalError

throwIfNothing :: InterpM (Maybe a) -> EvalError -> InterpM a
throwIfNothing valM err = do
  val' <- valM
  case val' of
    Nothing  -> throwError err
    Just val -> return val

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

wrapOp2 :: forall a b c. (FromVal a, FromVal b, ToVal c)
  => (a -> b -> c) -> (Val -> Val -> Maybe Val, [TypeRep])
wrapOp2 f = (
    \a' b' -> do
      a <- tryFromVal a'
      b <- tryFromVal b'
      return . toVal $ f a b,
    [typeOf (undefined :: a), typeOf (undefined :: b), typeOf (undefined :: c)]
  )

callOp2 :: (Evaluable a, Evaluable b)
  => String -> [(Val -> Val -> Maybe Val, [TypeRep])] -> a -> b -> InterpM Val
callOp2 opName fs a' b' = do
  a <- eval a'
  b <- eval b'
  doCall fs a b []
    where
      doCall :: [(Val -> Val -> Maybe Val, [TypeRep])] -> Val -> Val -> [[TypeRep]] -> InterpM Val
      doCall [] a b triedSigs = throwError OpCallTypeError {
          opName = opName,
          triedSigs = triedSigs,
          givenSig = [haskellTypeRep a, haskellTypeRep b]
        }
      doCall ((f, sig):fs) a b triedSigs = case f a b of
        Just v  -> return v
        Nothing -> doCall fs a b (sig:triedSigs)
