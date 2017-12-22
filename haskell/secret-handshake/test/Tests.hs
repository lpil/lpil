import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SecretHandshake (handshake)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = do
  it "wink for 1" $ handshake 1 `shouldBe` ["wink"]
  it "double blink for 10" $ handshake 2 `shouldBe` ["double blink"]
  it "close your eyes for 100" $ handshake 4 `shouldBe` ["close your eyes"]
  it "jump for 1000" $ handshake 8 `shouldBe` ["jump"]
  it "combine two actions" $ handshake 3 `shouldBe` ["wink", "double blink"]
  it "reverse two actions" $ handshake 19 `shouldBe` ["double blink", "wink"]
  it "reversing one action gives the same action"
    $          handshake 24
    `shouldBe` ["jump"]
  it "reversing no actions still gives no actions" $ handshake 16 `shouldBe` []
  it "all possible actions"
    $          handshake 15
    `shouldBe` ["wink", "double blink", "close your eyes", "jump"]
  it "reverse all possible actions"
    $          handshake 31
    `shouldBe` ["jump", "close your eyes", "double blink", "wink"]
  it "do nothing for zero" $ handshake 0 `shouldBe` []
  it "do nothing if lower 5 bits not set" $ handshake 32 `shouldBe` []
