module Language.Janus.InterpreterSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Language.Janus.AST
import           Language.Janus.Interpreter

main = hspec spec

spec = do
  describe "eval of Val" $ do
    it "correctly evaluates JUnit" $
      eval JUnit `shouldBe` Right JUnit
    it "correctly evaluates JBool" $
      (eval . JBool $ True) `shouldBe` (Right . JBool $ True)
    it "correctly evaluates JInt" $
      (eval . JInt $ 2) `shouldBe` (Right . JInt $ 2)
    it "correctly evaluates JDouble" $
      (eval . JDouble $ 3.0) `shouldBe` (Right . JDouble $ 3.0)
    it "correctly evaluates JChar" $
      (eval . JChar $ 'c') `shouldBe` (Right . JChar $ 'c')
    it "correctly evaluates JStr" $
      (eval . JStr $ "aaa") `shouldBe` (Right . JStr $ "aaa")
