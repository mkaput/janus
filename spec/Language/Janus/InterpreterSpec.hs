{-# LANGUAGE RecordWildCards #-}

module Language.Janus.InterpreterSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Language.Janus.AST
import           Language.Janus.Interpreter

main = hspec spec

spec = do
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


shouldEval' :: Evaluable a => EvalState -> a -> Val -> Expectation
shouldEval' st ast expected = do
  result <- runExceptT (evalStateT (eval ast) st)
  result `shouldBe` Right expected

shouldEval :: Evaluable a => a -> Val -> Expectation
shouldEval = shouldEval' emptyState

shouldEvalThrow' :: Evaluable a => EvalState -> a -> EvalError -> Expectation
shouldEvalThrow' st ast err = do
  result <- runExceptT (evalStateT (eval ast) st)
  result `shouldBe` Left err

shouldEvalThrow :: Evaluable a => a -> EvalError -> Expectation
shouldEvalThrow = shouldEvalThrow' emptyState

shouldEvalThrowTypeError' :: Evaluable a => EvalState -> a -> Expectation
shouldEvalThrowTypeError' st ast = do
  result <- runExceptT (evalStateT (eval ast) st)
  case result of
    Left OpCallTypeError{..} -> True `shouldBe` True
    x -> expectationFailure $
      "program did not throw TypeError, but instead returned:\n" ++ show x

shouldEvalThrowTypeError :: Evaluable a => a -> Expectation
shouldEvalThrowTypeError = shouldEvalThrowTypeError' emptyState
