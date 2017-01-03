{-# LANGUAGE RecordWildCards #-}

module Language.Janus.TestUtil where

import           Test.Hspec
import           Test.HUnit                 (assertFailure)
import           Test.QuickCheck

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Language.Janus.AST
import           Language.Janus.Interp

evalVar :: String -> InterpM Val
evalVar name = eval (LvalueExpr $ Path name)

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
