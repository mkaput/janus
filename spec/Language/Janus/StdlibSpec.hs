module Language.Janus.StdlibSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Language.Janus.TestUtil

import           Language.Janus.AST
import           Language.Janus.Interp
import           Language.Janus.Stdlib

main = hspec spec

spec =
  describe "abs" $ do
    it "works over ints" . testInterpM $ do
      importStdlib
      eval (CallExpr (LvalueExpr $ Path "abs") [toLiteralI (-4)]) `shouldInterp` JInt 4

    it "works over doubles" . testInterpM $ do
      importStdlib
      eval (CallExpr (LvalueExpr $ Path "abs") [toLiteralD (-4.0)]) `shouldInterp` JDouble 4.0
