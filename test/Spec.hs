import Test.Hspec
import Day01
import Day02

main :: IO ()
main = hspec $ do
    describe "Day01.calculateTotalFuel" $ do
      it "calculates the right amount of fuel" $ do
        Day01.calculateTotalFuel Day01.modules `shouldBe` (4866824 :: Int)

      it "handles an empty list" $ do
        Day01.calculateTotalFuel [] `shouldBe` (0 :: Int)