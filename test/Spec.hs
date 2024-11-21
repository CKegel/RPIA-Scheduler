import Test.Hspec

main :: IO ()
main = hspec $ describe "Example Test" $ do
  it "suceeds" True
  it "expects false" $ do
    False `shouldBe` alwaysFalse


alwaysFalse :: Bool
alwaysFalse = False
