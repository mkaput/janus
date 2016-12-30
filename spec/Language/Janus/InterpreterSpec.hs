{-# LANGUAGE RecordWildCards #-}

module Language.Janus.InterpreterSpec where

import           Test.Hspec
import           Test.HUnit                 (assertFailure)
import           Test.QuickCheck

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.List                  (sort)

import           Language.Janus.AST
import           Language.Janus.Interp

main = hspec spec

spec = do
  describe "maxObjCount" . it "> 0" $ maxObjCount `shouldSatisfy` (> 0)


  describe "GC" $ do
    it "works" . testInterpM $ do
      ptr <- memAlloc (JInt 42)

      liftIO $ ptr `shouldBe` 0
      memIsFree ptr `shouldInterp` False
      memGetVal ptr `shouldInterp` JInt 42
      memGetRc ptr `shouldInterp` 0

      rcIncr ptr
      memGetRc ptr `shouldInterp` 1

      rcDecr ptr
      memIsFree ptr `shouldInterp` True


    it "memGetVal throws error for invalid pointer" $
      let m = runInterpM $ memGetVal 0
      in m `shouldReturn` Left (InvalidPointer 0)


  describe "symbol manipulating" $
    it "works" . testInterpM $ do
      ptrA <- memAlloc JUnit
      putSymbol "a" ptrA
      evalSymbol "a" `shouldInterp` JUnit

      ptrB <- memAlloc $ JInt 42
      putSymbol "a" ptrB
      memIsFree ptrA `shouldInterp` True
      memIsFree ptrB `shouldInterp` False
      evalSymbol "a" `shouldInterp` JInt 42


  describe "allSymbols" $ do
    it "works" . testInterpM $ do
      ptr <- memAlloc JUnit
      putSymbol "a" ptr
      putSymbol "b" ptr
      pushScope
      putSymbol "c" ptr
      putSymbol "d" ptr
      (sort <$> allSymbols) `shouldInterp` ["a", "b", "c", "d"]

    it "returns [] for empty state" . testInterpM $
      allSymbols `shouldInterp` []


  describe "eval of Val" $ do
    it "correctly evaluates JUnit" $ JUnit `shouldEval` JUnit
    it "correctly evaluates JBool" $ JBool True `shouldEval` JBool True
    it "correctly evaluates JInt" $ JInt 2 `shouldEval` JInt 2
    it "correctly evaluates JDouble" $ JDouble 3.0 `shouldEval` JDouble 3.0
    it "correctly evaluates JChar" $ JChar 'c' `shouldEval` JChar 'c'
    it "correctly evaluates JStr" $ JStr "aaa" `shouldEval` JStr "aaa"

  describe "eval of EvalError" .
    it "correctly evaluates to interpreter error" $
      InternalError "foo" `shouldEvalThrow` InternalError "foo"

  describe "eval of !x" $ do
    it "!True == False" $ NotExpr (toLiteral True) `shouldEval` toVal False
    it "!False == True" $ NotExpr (toLiteral False) `shouldEval` toVal True
    it "!1 type errors" . shouldEvalThrowTypeError $ NotExpr (toLiteralI 1)

  describe "eval of ~x" $ do
    it "~True == False" $ BitNotExpr (toLiteral True) `shouldEval` toVal False
    it "~False == True" $ BitNotExpr (toLiteral False) `shouldEval` toVal True
    it "~5 == -6" $ BitNotExpr (toLiteralI 5) `shouldEval` toValI (-6)
    it "~5.0 type errors" . shouldEvalThrowTypeError $ BitNotExpr (toLiteralD 5.0)

  describe "eval of +x" $ do
    it "+1 == 1" $ PlusExpr (toLiteralI 1) `shouldEval` toValI 1
    it "+1.0 == 1.0" $ PlusExpr (toLiteralD 1.0) `shouldEval` toValD 1.0
    it "+True type errors" . shouldEvalThrowTypeError $ PlusExpr (toLiteral True)

  describe "eval of -x" $ do
    it "-(1) == -1" $ NegExpr (toLiteralI 1) `shouldEval` toValI (-1)
    it "-(1.0) == -1.0" $ NegExpr (toLiteralD 1.0) `shouldEval` toValD (-1.0)
    it "-True type errors" . shouldEvalThrowTypeError $ NegExpr (toLiteral True)

  describe "eval of x ** n" $ do
    it "2 ** 5 == 32" $ ExpExpr (toLiteralI 2) (toLiteralI 5) `shouldEval` toValI 32
    it "2.0 ** 5.0 == 32.0" $ ExpExpr (toLiteralD 2.0) (toLiteralD 5.0) `shouldEval` toValD 32.0
    it "2 ** 5.0 type errors" . shouldEvalThrowTypeError $ ExpExpr (toLiteralI 2) (toLiteralD 5.0)

  describe "eval of a * b" $ do
    it "2 * 5 == 10" $ MulExpr (toLiteralI 2) (toLiteralI 5) `shouldEval` toValI 10
    it "2.0 * 5.0 == 10.0" $ MulExpr (toLiteralD 2.0) (toLiteralD 5.0) `shouldEval` toValD 10.0
    it "2 * 5.0 type errors" . shouldEvalThrowTypeError $ MulExpr (toLiteralI 2) (toLiteralD 5.0)

  describe "eval of a + b" $ do
    it "'a' + 'a' type errors" . shouldEvalThrowTypeError $ AddExpr (toLiteral 'a') (toLiteral 'a')
    it "'a' + \"a\" == \"aa\"" $ AddExpr (toLiteral 'a') (toLiteral "a") `shouldEval` toVal "aa"
    it "\"a\" + 'a' == \"aa\"" $ AddExpr (toLiteral "a") (toLiteral 'a') `shouldEval` toVal "aa"
    it "\"a\" + \"a\" == \"aa\"" $ AddExpr (toLiteral "a") (toLiteral "a") `shouldEval` toVal "aa"


testInterpM :: InterpM a -> Expectation
testInterpM m = do
  result <- runInterpM m
  case result of
    Right _  -> True `shouldBe` True
    Left err -> assertFailure $ show err

shouldInterp :: (Eq a, Show a) => InterpM a -> a -> InterpM ()
shouldInterp given expected = void $ do
  g <- given
  liftIO $ g `shouldBe` expected

shouldEval :: Evaluable a => a -> Val -> Expectation
shouldEval ast expected = run ast `shouldReturn` Right expected

shouldEvalThrow :: Evaluable a => a -> EvalError -> Expectation
shouldEvalThrow ast err =  run ast `shouldReturn` Left err

shouldEvalThrowTypeError :: Evaluable a => a -> Expectation
shouldEvalThrowTypeError ast = do
  result <- run ast
  case result of
    Left OpCallTypeError{..} -> True `shouldBe` True
    x -> expectationFailure $
      "program did not throw TypeError, but instead returned:\n" ++ show x
