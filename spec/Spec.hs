import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Hello World" $ do
        it "works" $
            True

        it "doesn't work" $ do
            False

    describe "read" $ do
        it "is inverse to show" . property $
            \x -> (read . show) x == (x :: Int)
