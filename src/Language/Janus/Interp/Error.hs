module Language.Janus.Interp.Error (
  EvalError(..)
) where

import           Data.Typeable          (TypeRep)
import           Text.Printf            (printf)

import           Language.Janus.AST.Val

data EvalError = OpCallTypeError {
                  opName    :: String,
                  triedSigs :: [[TypeRep]],
                  givenSig  :: [TypeRep]
                }
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

  show (InternalError msg) = "Internal error: " ++ msg

  show (InvalidPointer ptr) = printf "Invalid pointer: 0x%08x" (getAddress ptr)

  show OutOfMemory = "out of memory"

  show (UndefinedSymbol name) = "undefined symbol " ++ name
