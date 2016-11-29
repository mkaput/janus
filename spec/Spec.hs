import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Hello World" $ do
        it "works" $
            True

    describe "read" $ do
        it "is inverse to show" . property $
            \x -> (read . show) x == (x :: Int)
