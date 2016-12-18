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

  describe "eval of NotExpr" $ do
    it "!True == False" $ NotExpr (toLiteral True) `shouldEval` toVal False
    it "!False == True" $ NotExpr (toLiteral False) `shouldEval` toVal True
    it "!1 type errors" $
      NotExpr (toLiteral (1 :: Int)) `shouldEvalThrow` TypeError


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
