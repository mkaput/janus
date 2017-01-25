module Language.Janus.ParserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Language.Janus.AST
import           Language.Janus.Parser

main = hspec spec

spec = do
  describe "statements" $ do
    it "let decl" $
      parseStatement "let ala = 2;" `shouldBe` Right (LetDecl "ala" (LiteralExpr (JInt 2)))
