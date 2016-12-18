module Language.Janus.InterpreterSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Language.Janus.AST
import           Language.Janus.Interpreter

main = hspec spec

doTest :: Evaluable a => EvalState -> Val -> a -> Expectation
doTest st expected actual = do
  result <- runExceptT (evalStateT (eval actual) st)
  result `shouldBe` Right expected

doTestE :: Evaluable a => Val -> a -> Expectation
doTestE = doTest emptyState

spec = do
  describe "eval of Val" $ do
    it "correctly evaluates JUnit" $
      doTestE JUnit JUnit
    it "correctly evaluates JBool" $
      doTestE (JBool True) (JBool True)
    it "correctly evaluates JInt" $
      doTestE (JInt 2) (JInt 2)
    it "correctly evaluates JDouble" $
      doTestE (JDouble 3.0) (JDouble 3.0)
    it "correctly evaluates JChar" $
      doTestE (JChar 'c') (JChar 'c')
    it "correctly evaluates JStr" $
      doTestE (JStr "aaa") (JStr "aaa")
