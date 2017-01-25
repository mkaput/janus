module Language.Janus.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Language.Janus.AST
import Language.Janus.Parser

main = hspec spec

spec = do
  describe "Statements" $ do

    it "letDecl" $
      parseStatement "let ala = 2;" == Right (LetDecl "ala" (LiteralExpr (JInt 2)))
