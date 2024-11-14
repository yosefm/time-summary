import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Utility functions" $ do
        it "formats float hours without overflow" $ do
            formatFloatHours 1.99999 `shouldBe` "01:59"

