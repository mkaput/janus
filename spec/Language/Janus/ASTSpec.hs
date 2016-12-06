module Language.Janus.ASTSpec where

import Test.Hspec
import Test.QuickCheck

import Language.Janus.AST

main = hspec spec

spec = do
  describe "Val" $ do
    it "is equatable over ints" . property $
      \i1 i2 -> if i1 == i2
        then JInt i1 == JInt i2
        else JInt i1 /= JInt i2

    it "is equatable over doubles" . property $
      \d1 d2 -> if d1 == d2
        then JDouble d1 == JDouble d2
        else JDouble d1 /= JDouble d2

    it "is equatable over booleans" . property $
      \b1 b2 -> if b1 == b2
        then JBool b1 == JBool b2
        else JBool b1 /= JBool b2

    it "is equatable over chars" . property $
      \c1 c2 -> if c1 == c2
        then JChar c1 == JChar c2
        else JChar c1 /= JChar c2

    it "is equatable over strings" . property $
      \s1 s2 -> if s1 == s2
        then JStr s1 == JStr s2
        else JStr s1 /= JStr s2

    it "is not equatable between ints and doubles" . property $
      \a b -> JInt a /= JDouble b

    it "is not equatable between ints and booleans" . property $
      \a b -> JInt a /= JBool b

    it "is ordered over ints" . property $
      \a b -> (a `compare` b) == (JInt a `compare` JInt b)

    it "is ordered over doubles" . property $
      \a b -> (a `compare` b) == (JDouble a `compare` JDouble b)

    it "is ordered over booleans" . property $
      \a b -> (a `compare` b) == (JBool a `compare` JBool b)

    it "is ordered over chars" . property $
      \a b -> (a `compare` b) == (JChar a `compare` JChar b)

    it "is ordered over strings" . property $
      \a b -> (a `compare` b) == (JStr a `compare` JStr b)

  describe "showVal" $ do
    it "shows inner value just like 'show' on ints" . property $
      \x -> show x == showVal (JInt x)

    it "shows inner value just like 'show' on doubles" . property $
      \x -> show x == showVal (JDouble x)

    it "shows inner value just like 'show' on booleans" . property $
      \x -> show x == showVal (JBool x)

    it "shows inner value just like 'show' on chars" . property $
      \x -> show x == showVal (JChar x)

    it "shows inner value just like 'show' on strings" . property $
      \x -> show x == showVal (JStr x)
